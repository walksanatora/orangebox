--- orangebox 1.0
-- By JackMacWindows
--
-- @module orangebox
--
-- This module allows you to easily create and manage special ComputerCraft
-- containers called "boxes". These boxes contain their own separate execution
-- environment, completely sequestered from the caller's environment. This allows
-- effective "virtual machines" running CraftOS.
--
-- orangebox supports:
-- * Custom BIOSes
-- * Virtual filesystems (including a basic serialize/deserialize storage format)
-- * Custom terminals
-- * Queueing arbitrary events outside the box
-- * Mounts between the real FS and inner FS
-- * Peripheral exports
-- * API exports
-- * Custom configuration (for some options)
-- * Redstone interception
--
-- To create a new box, use orangebox:new(). Then call box:resume() to start
-- execution. The resume method exits either when the computer shuts down or
-- there are no more events waiting in the queue. To replenish the events and
-- continue execution, use box:queueEvent(event, ...) to push more events to the
-- queue (usually from os.pullEventRaw). The box.running property can be used to
-- determine whether the environment is running/waiting for events.
--
-- To load the root filesystem from a file, you can use box:loadVFS(path). This
-- takes the path to a serialized table file, and sets that path as the location
-- to write new data to. It also initializes the contents of the disk with the
-- current contents of the file if available. If you want to write your own
-- filesystem instead of using the built-in VFS functionality, you can set
-- box.disk to a table containing the filesystem hierarchy as a key-value table,
-- and box:syncfs to a function that is called each time the filesystem is
-- written to.
--
-- Any changes to the environment from the default (such as terminals) should be
-- done before calling box:resume() for the first time. If this is not possible,
-- you can call box:reloadenv() to reload the environment (however, this may or
-- may not work properly depending on the workings of the embedded OS).
--
-- Here's an example program demonstrating how to use orangebox:
--
--   local file = fs.open("bios.lua", "rb")
--   local vm = orangebox:new(file.readAll())
--   file.close()
--   vm:loadVFS("filesystem.vfs")
--   vm:mount("/realFS", "/")
--   vm:resume()
--   while vm.running do
--       vm:queueEvent(os.pullEventRaw())
--       vm:resume()
--   end


-- MIT License
-- 
-- Copyright (c) 2021 JackMacWindows
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

local expect = require "cc.expect".expect

local orangebox = {}

local debugger = peripheral.find("debugger")

local function print(message)
    _ = debugger and debugger.print(message)
end

local found_libdeflate, libdeflate = pcall(require, "LibDeflate")

--- This table converts side names to side numbers.
orangebox.sideNames = {
    top = 1,
    bottom = 2,
    left = 3,
    right = 4,
    front = 5,
    back = 6,
}

--- This table converts side numbers to side names.
orangebox.sideNumbers = {
    "top",
    "bottom",
    "left",
    "right",
    "front",
    "back",
}

--- Creates a new box.
-- @tparam string|nil bios The BIOS to use. If unset, use loadBIOS later.
-- @tparam table|nil The disk data. If unset, defaults to an empty disk.
-- @treturn orangebox A new box instance.
function orangebox:new(bios, disk)
    expect(1, bios, "string", "nil")
    expect(2, disk, "table", "nil")
    local obj = setmetatable({
        coro = nil,
        bios = bios,
        disk = disk or {},
        syncfs = function() end,
        compression_enabled = false,
        libdeflate_loaded = false,
        fs_dirty = false,
        sync_last = os.epoch("utc"),
        sync_cooldown_ms = 10000,
        term = (
            function ()
                local a = term.current()
                a.nativePaletteColor = term.nativePaletteColor
                a.nativePaletteColour = term.nativePaletteColour
                return a
            end)(),
        eventQueue = {},
        mounts = {[{"rom"}] = "rom"},
        peripherals = {},
        apis = {bit32 = bit32},
        config = {
            http_enable = http ~= nil,
            http_websocket_enable = http ~= nil and http.websocket ~= nil,
            disable_lua51_features = _CC_DISABLE_LUA51_FEATURES,
            default_computer_settings = _CC_DEFAULT_SETTINGS,
            maximumFilesOpen = 128,
        },
        redstone = {
            input = {0, 0, 0, 0, 0, 0},
            output = {0, 0, 0, 0, 0, 0},
            bundledInput = {0, 0, 0, 0, 0, 0},
            bundledOutput = {0, 0, 0, 0, 0, 0},
        },
        timers = {},
        alarms = {},
        running = false,
        id = 0,
        label = nil,
        startTime = nil,
        filter = nil,
        openFiles = 0,
    }, {__index = orangebox})
    if bios then
        obj.fn = load(bios, "=bios.lua", "t", obj:makeenv())
        obj.coro = coroutine.create(obj.fn)
    end
    return obj
end

local function checkMount(mounts, path)
    local parts = {}
    for p in fs.combine(path):gmatch("[^/]+") do parts[#parts + 1] = p end
    for k, v in pairs(mounts) do
        local ok = true
        for i, p in ipairs(k) do if parts[i] ~= p then ok = false break end end
        if ok then
            for _ = 1, #k do table.remove(parts, 1) end
            return true, fs.combine(v, table.concat(parts, "/"))
        end
    end
    return false
end

local function getPath(tab, path)
    for p in fs.combine(path):gmatch("[^/]+") do
        tab = tab[p]
        if tab == nil then return nil end
    end
    return tab
end

local function getPath_mkdir(tab, path)
    for p in fs.combine(path):gmatch("[^/]+") do
        if type(tab) == "string" then return nil
        elseif tab[p] == nil then tab[p] = {} end
        tab = tab[p]
    end
    return tab
end

local function deepcopy(orig)
    local orig_type = type(orig)
    local copy
    if orig_type == 'table' then
        copy = {}
        for orig_key, orig_value in next, orig, nil do
            copy[deepcopy(orig_key)] = deepcopy(orig_value)
        end
        setmetatable(copy, deepcopy(getmetatable(orig)))
    else -- number, string, boolean, etc
        copy = orig
    end
    return copy
end

local function aux_find(parts, t, mounts)
    if #parts == 0 then return type(t) == "table" and "" or t elseif type(t) ~= "table" then return nil end
    local parts2 = {}
    for i, v in ipairs(parts) do parts2[i] = v end
    local name = table.remove(parts2, 1)
    local retval = {}
    if t then for k, v in pairs(t) do if k:match("^" .. name:gsub("([%%%.])", "%%%1"):gsub("%*", "%.%*") .. "$") then retval[k] = aux_find(parts2, v, mounts[k]) end end end
    if mounts then for k, v in pairs(mounts) do if k[1]:match("^" .. name:gsub("([%%%.])", "%%%1"):gsub("%*", "%.%*") .. "$") then
        if #k == 1 then
            local r = fs.find(fs.combine(v, table.concat(parts2, "/")))
            retval[k[1]] = {}
            for _, w in ipairs(r) do retval[k[1]][fs.combine((w:gsub(v, "")))] = "" end
        else retval[k[1]] = aux_find(parts2, t[k[1]], {table.unpack(k, 2)}) end 
    end end end
    return retval
end

local function combineKeys(t, prefix)
    prefix = prefix or ""
    if t == nil then return {} end
    local retval = {}
    for k, v in pairs(t) do
        if type(v) == "string" then table.insert(retval, prefix .. k)
        else for _, w in ipairs(combineKeys(v, prefix .. k .. "/")) do table.insert(retval, w) end end
    end
    return retval
end

local fs, os, peripheral, getfenv, setfenv, load, loadstring = fs, os, peripheral, getfenv, setfenv, load, loadstring

--TODO: implement gcinfo() collectgarbage(), newproxy()
--http: addListener,checkUrlAsync,head,listen,options,patch,put,removeListener,trace,websocketAsync
--os: about
--CCPC: periphemu,mounter,term.screenshot
--- Creates the environment for the box. This is mostly an internal function,
-- but it's exported as part of the class.
-- @treturn table A new environment for the box.
function orangebox:makeenv()
    local env
    env = {
        assert = assert,
        error = error,
        getfenv = function(n)
            local e = getfenv(n)
            if e and e._G == _G then return nil end
            return e
        end,
        getmetatable = getmetatable,
        ipairs = ipairs,
        load = function(_chunk, _name, _mode, _env)
            if not _env then _env = env end
            return load(_chunk, _name, _mode, _env)
        end,
        loadstring = function(chunk, name)
            local fn, err = loadstring(chunk, name)
            if fn then setfenv(fn, env) end
            return fn, err
        end,
        next = next,
        pairs = pairs,
        pcall = pcall,
        rawequal = rawequal,
        rawget = rawget,
        rawset = rawset,
        select = select,
        setfenv = setfenv,
        setmetatable = setmetatable,
        tonumber = tonumber,
        tostring = tostring,
        type = type,
        unpack = unpack,
        _VERSION = _VERSION,
        xpcall = xpcall,
        _HOST = "ComputerCraft 1.95.2 (orangebox 1.0)",
        _CC_DEFAULT_SETTINGS = self.config.default_computer_settings,
        _CC_DISABLE_LUA51_FEATURES = self.config.disable_lua51_features,
        coroutine = coroutine,
        math = math,
        string = string,
        table = table,
        term = self.term,
        utf8 = utf8,
        fs = {
            list = function(path)
                expect(1, path, "string")
                local isMount, mountPath = checkMount(self.mounts, path)
                if isMount then return fs.list(mountPath) end
                local tab = getPath(self.disk, path)
                if type(tab) ~= "table" then error(path .. ": Not a directory", 2) end
                local retval = {}
                for k in pairs(tab) do retval[#retval + 1] = k end
                local parts = {}
                for p in fs.combine(path):gmatch("[^/]+") do parts[#parts + 1] = p end
                for k in pairs(self.mounts) do
                    if #parts == #k - 1 then
                        local ok = true
                        for i, p in ipairs(k) do if parts[i] ~= p and i < #k then ok = false break end end
                        if ok then retval[#retval + 1] = k[#k] end
                    end
                end
                table.sort(retval)
                return retval
            end,
            exists = function(path)
                expect(1, path, "string")
                local isMount, mountPath = checkMount(self.mounts, path)
                if isMount then return fs.exists(mountPath) end
                return getPath(self.disk, path) ~= nil
            end,
            isDir = function(path)
                expect(1, path, "string")
                local isMount, mountPath = checkMount(self.mounts, path)
                if isMount then return fs.isDir(mountPath) end
                local tab = getPath(self.disk, path)
                return type(tab) == "table"
            end,
            isReadOnly = function(path)
                expect(1, path, "string")
                local isMount, mountPath = checkMount(self.mounts, path)
                if isMount then return fs.isReadOnly(mountPath) end
                return false -- todo
            end,
            getName = fs.getName,
            getDrive = function(path)
                expect(1, path, "string")
                local isMount, mountPath = checkMount(self.mounts, path)
                if isMount then return fs.getDrive(mountPath) end
                return "hdd"
            end,
            getSize = function(path)
                expect(1, path, "string")
                local isMount, mountPath = checkMount(self.mounts, path)
                if isMount then return fs.getSize(mountPath) end
                local tab = getPath(self.disk, path)
                if tab == nil then error(path .. ": No such file") end
                if type(tab) == "table" then return 0
                else return #tab end
            end,
            getFreeSpace = function(path)
                expect(1, path, "string")
                local isMount, mountPath = checkMount(self.mounts, path)
                if isMount then return fs.getFreeSpace(mountPath) end
                return 1000000
            end,
            makeDir = function(path)
                expect(1, path, "string")
                local isMount, mountPath = checkMount(self.mounts, path)
                if isMount then return fs.makeDir(mountPath) end
                local tab = self.disk
                for p in fs.combine(path):gmatch("[^/]+") do
                    if tab[p] == nil then tab[p] = {}
                    elseif type(tab[p]) == "string" then
                        self:syncfs()
                        error(path .. ": File already exists", 2)
                    end
                    tab = tab[p]
                end
                self:syncfs()
            end,
            move = function(fromPath, toPath)
                expect(1, fromPath, "string")
                local isFromMount, fromMountPath = checkMount(self.mounts, fromPath)
                local isToMount, toMountPath = checkMount(self.mounts, toPath)
                if isFromMount and isToMount then return fs.move(fromMountPath, toMountPath)
                elseif isFromMount and not isToMount then
                    local function move(from, toTab, idx, level)
                        if fs.isReadOnly(from) then error(fromPath .. ": Access denied", level) end
                        if fs.isDir(from) then
                            toTab[idx] = {}
                            for _, v in ipairs(fs.list(from)) do
                                move(fs.combine(from, v), toTab[idx], level + 1)
                            end
                            fs.delete(from)
                        else
                            local file, err = fs.open(from, "rb")
                            if file == nil then self:syncfs() error(err, level) end
                            toTab[idx] = file.readAll()
                            file.close()
                            fs.delete(from)
                        end
                    end
                    move(fromMountPath, getPath_mkdir(self.disk, fs.getDir(toPath)), fs.getName(toPath), 3)
                    self:syncfs()
                elseif not isFromMount and isToMount then
                    local function move(fromTab, idx, to, level)
                        if fs.isReadOnly(to) then error(toPath .. ": Access denied", level) end
                        if type(fromTab[idx]) == "table" then
                            fs.makeDir(to)
                            for k in pairs(fromTab[idx]) do
                                move(fromTab[idx], k, fs.combine(to, k), level + 1)
                            end
                            fromTab[idx] = nil
                        else
                            local file, err = fs.open(to, "wb")
                            if file == nil then self:syncfs() error(err, level) end
                            file.write(fromTab[idx])
                            file.close()
                            fromTab[idx] = nil
                        end
                    end
                    move(getPath(self.disk, fs.getDir(fromPath)), fs.getName(fromPath), toPath, 3)
                    self:syncfs()
                else
                    getPath_mkdir(self.disk, fs.getDir(toPath))[fs.getName(toPath)] = getPath(self.disk, fromPath)
                    getPath(self.disk, fs.getDir(fromPath))[fs.getName(fromPath)] = nil
                    self:syncfs()
                end
            end,
            copy = function(fromPath, toPath)
                expect(1, fromPath, "string")
                expect(2, toPath, "string")
                local isFromMount, fromMountPath = checkMount(self.mounts, fromPath)
                local isToMount, toMountPath = checkMount(self.mounts, toPath)
                if isFromMount and isToMount then return fs.copy(fromMountPath, toMountPath)
                elseif isFromMount and not isToMount then
                    local function copy(from, toTab, idx, level)
                        if fs.isDir(from) then
                            toTab[idx] = {}
                            for _, v in ipairs(fs.list(from)) do
                                copy(fs.combine(from, v), toTab[idx], level + 1)
                            end
                        else
                            local file, err = fs.open(from, "rb")
                            if file == nil then self:syncfs() error(err, level) end
                            toTab[idx] = file.readAll()
                            file.close()
                        end
                    end
                    copy(fromMountPath, getPath_mkdir(self.disk, fs.getDir(toPath)), fs.getName(toPath), 3)
                    self:syncfs()
                elseif not isFromMount and isToMount then
                    local function copy(fromTab, idx, to, level)
                        if fs.isReadOnly(to) then error(toPath .. ": Access denied", level) end
                        if type(fromTab[idx]) == "table" then
                            fs.makeDir(to)
                            for k in pairs(fromTab[idx]) do
                                copy(fromTab[idx], k, fs.combine(to, k), level + 1)
                            end
                        else
                            local file, err = fs.open(to, "wb")
                            if file == nil then self:syncfs() error(err, level) end
                            file.write(fromTab[idx])
                            file.close()
                        end
                    end
                    copy(getPath(self.disk, fs.getDir(fromPath)), fs.getName(fromPath), toPath, 3)
                    self:syncfs()
                else
                    getPath_mkdir(self.disk, fs.getDir(toPath))[fs.getName(toPath)] = deepcopy(getPath(self.disk, fromPath))
                    self:syncfs()
                end
            end,
            delete = function(path)
                expect(1, path, "string")
                local isMount, mountPath = checkMount(self.mounts, path)
                if isMount then return fs.delete(mountPath) end
                local tab = self.disk
                for p in fs.combine(path, ".."):gmatch("[^/]+") do
                    if tab[p] == nil or type(tab[p]) == "string" then error(path .. ": No such file or directory", 2) end
                    tab = tab[p]
                end
                tab[fs.getName(path)] = nil
                self:syncfs()
            end,
            combine = fs.combine,
            complete = fs.complete,
            open = function(path, mode)
                expect(1, path, "string")
                expect(2, mode, "string")
                local isMount, mountPath = checkMount(self.mounts, path)
                if isMount then return fs.open(mountPath, mode) end
                if self.openFiles >= self.config.maximumFilesOpen then return nil, "Too many open files" end
                self.openFiles = self.openFiles + 1
                if mode:find("r") then
                    if mode:find("b") then
                        local tab = getPath(self.disk, path)
                        if type(tab) ~= "string" then return nil, "No such file" end
                        local pos = 1
                        local closed = false
                        return {
                            readLine = function(withTrailing)
                                if closed then error("file is already closed", 2) end
                                if pos > #tab then return end
                                local str, endPos = tab:match(withTrailing and "([^\n]*\n?)()" or "([^\n]*)\n?()", pos)
                                pos = str and endPos or #tab + 1
                                return str
                            end,
                            readAll = function()
                                if closed then error("file is already closed", 2) end
                                if #tab == 0 and pos == 1 then
                                    pos = 2
                                    return ""
                                end
                                if pos > #tab then return end
                                local oldPos = pos
                                pos = #tab + 1
                                return tab:sub(oldPos)
                            end,
                            read = function(count)
                                expect(1, count, "number", "nil")
                                if closed then error("file is already closed", 2) end
                                if pos > #tab then return end
                                if count == nil then
                                    pos = pos + 1
                                    return tab:byte(pos - 1)
                                else
                                    local oldPos = pos
                                    pos = pos + count
                                    return tab:sub(oldPos, pos - 1)
                                end
                            end,
                            close = function()
                                if closed then error("file is already closed", 2) end
                                closed = true
                                self.openFiles = self.openFiles - 1
                            end,
                            seek = function(whence, offset)
                                if closed then error("file is already closed", 2) end
                                expect(1, whence, "string", "nil")
                                expect(2, offset, "number", "nil")
                                whence = whence or "cur"
                                offset = offset or 0
                                if whence == "set" then pos = offset
                                elseif whence == "cur" then pos = pos + offset
                                elseif whence == "end" then pos = #tab - offset
                                else error("bad argument #1 (invalid option " .. whence .. ")", 2) end
                                return pos
                            end,
                        }
                    else
                        local tab = getPath(self.disk, path)
                        if type(tab) ~= "string" then return nil, "No such file" end
                        local oldtab = tab
                        tab = ""
                        for _, c in utf8.codes(oldtab) do tab = tab .. (c > 255 and "?" or string.char(c)) end
                        tab = tab:gsub("\r\n", "\n")
                        local pos = 1
                        local closed = false
                        return {
                            readLine = function(withTrailing)
                                if closed then error("file is already closed", 2) end
                                if pos > #tab then return end
                                local str, endPos = tab:match(withTrailing and "([^\n]*\n?)()" or "([^\n]*)\n?()", pos)
                                pos = str and endPos or #tab + 1
                                return str
                            end,
                            readAll = function()
                                if closed then error("file is already closed", 2) end
                                if #tab == 0 and pos == 1 then
                                    pos = 2
                                    return ""
                                end
                                if pos > #tab then return end
                                local oldPos = pos
                                pos = #tab + 1
                                return tab:sub(oldPos)
                            end,
                            read = function(count)
                                if closed then error("file is already closed", 2) end
                                if pos > #tab then return end
                                expect(1, count, "number", "nil")
                                count = count or 1
                                local oldPos = pos
                                pos = pos + count
                                return tab:sub(oldPos, pos - 1)
                            end,
                            close = function()
                                if closed then error("file is already closed", 2) end
                                closed = true
                                self.openFiles = self.openFiles - 1
                            end,
                        }
                    end
                elseif mode:find("w") or mode:find("a") then
                    if mode:find('b') then
                        local dir = getPath_mkdir(self.disk, fs.getDir(path))
                        if dir == nil then return nil, "File exists" end
                        local name = fs.getName(path)
                        if type(dir[name]) == "table" then return nil, "Directory exists" end
                        local data = ""
                        local closed = false
                        if mode == "wb" or dir[name] == nil then
                            dir[name] = data
                            self:syncfs()
                        elseif mode == "ab" then
                            if not dir[name] then return nil, "No such file" end
                            data = dir[name]
                        end
                        local pos = #data
                        return {
                            write = function(value)
                                if closed then error("file is already closed", 2) end
                                if type(value) == "number" then value = string.char(value) end
                                data = data:sub(1, pos) .. value .. data:sub(pos + #value + 1)
                                pos = pos + #value
                            end,
                            writeLine = function(value)
                                if closed then error("file is already closed", 2) end
                                if type(value) == "number" then value = string.char(value) end
                                data = data:sub(1, pos) .. value .. "\n" .. data:sub(pos + #value + 2)
                                pos = pos + #value + 1
                            end,
                            flush = function()
                                if closed then error("file is already closed", 2) end
                                dir[name] = data
                                self:syncfs()
                            end,
                            close = function()
                                if closed then error("file is already closed", 2) end
                                dir[name] = data
                                self:syncfs()
                                closed = true
                                self.openFiles = self.openFiles - 1
                            end,
                            seek = function(whence, offset)
                                if closed then error("file is already closed", 2) end
                                expect(1, whence, "string", "nil")
                                expect(2, offset, "number", "nil")
                                whence = whence or "cur"
                                offset = offset or 0
                                if whence == "set" then pos = offset
                                elseif whence == "cur" then pos = pos + offset
                                elseif whence == "end" then pos = #tab - offset
                                else error("bad argument #1 (invalid option " .. whence .. ")", 2) end
                                return pos
                            end,
                        }
                    else
                        local dir = getPath_mkdir(self.disk, fs.getDir(path))
                        if dir == nil then return nil, "File exists" end
                        local name = fs.getName(path)
                        if type(dir[name]) == "table" then return nil, "Directory exists" end
                        local data = ""
                        local closed = false
                        if mode == "w" or dir[name] == nil then
                            dir[name] = data
                            self:syncfs()
                        elseif mode == "a" then
                            if not dir[name] then return nil, "No such file" end
                            data = dir[name]
                            local oldtab = data
                            data = ""
                            for _, c in utf8.codes(oldtab) do data = data .. (c > 255 and "?" or string.char(c)) end
                            data = data:gsub("\r\n", "\n")
                        end
                        return {
                            write = function(value)
                                if closed then error("file is already closed", 2) end
                                data = data .. utf8.char(tostring(value):byte(1, -1))
                            end,
                            writeLine = function(value)
                                if closed then error("file is already closed", 2) end
                                data = data .. utf8.char(tostring(value):byte(1, -1)) .. "\n"
                            end,
                            flush = function()
                                if closed then error("file is already closed", 2) end
                                dir[name] = data
                                self:syncfs()
                            end,
                            close = function()
                                if closed then error("file is already closed", 2) end
                                dir[name] = data
                                self:syncfs()
                                closed = true
                                self.openFiles = self.openFiles - 1
                            end,
                        }
                    end
                else return nil, "Invalid mode" end
            end,
            find = function(wildcard)
                expect(1, wildcard, "string")
                local parts = {}
                for p in wildcard:gmatch("[^/]+") do parts[#parts + 1] = p end
                local retval = {}
                for _, v in ipairs(combineKeys(aux_find(parts, self.disk, self.mounts))) do table.insert(retval, v) end
                table.sort(retval)
                return retval
            end,
            getDir = fs.getDir,
            attributes = function(path)
                expect(1, path, "string")
                local isMount, mountPath = checkMount(self.mounts, path)
                if isMount then return fs.attributes(mountPath) end
                local tab = getPath(self.disk, path)
                return {
                    size = type(tab) == "table" and 0 or #tab,
                    isDir = type(tab) == "table",
                    isReadOnly = false,
                    created = 0,
                    modified = 0,
                }
            end,
            getCapacity = function(path)
                expect(1, path, "string")
                local isMount, mountPath = checkMount(self.mounts, path)
                if isMount then return fs.getCapacity(mountPath) end
                return 1000000
            end,
            sync = function()
                self:syncfs()
            end,
        },
        http = self.config.http_enable and {
            request = http.request,
            checkURL = http.checkURLAsync,
            get = http.get,
            post = http.post,
            websocket = self.config.http_websocket_enable and http.websocketAsync or nil,
        } or nil,
        os = {
            queueEvent = function(event, ...)
                expect(1, event, "string")
                self.eventQueue[#self.eventQueue + 1] = {event, ...}
            end,
            startTimer = function(timeout)
                expect(1, timeout, "number")
                local id = os.startTimer(timeout)
                if id ~= nil then self.timers[id] = true end
                return id
            end,
            cancelTimer = function(id)
                expect(1, id, "number")
                if not self.timers[id] then return end
                os.cancelTimer(id)
                self.timers[id] = nil
            end,
            setAlarm = function(timeout)
                expect(1, timeout, "number")
                local id = os.setAlarm(timeout)
                if id ~= nil then self.alarms[id] = true end
                return id
            end,
            cancelAlarm = function(id)
                expect(1, id, "number")
                if not self.alarms[id] then return end
                os.cancelAlarm(id)
                self.alarms[id] = nil
            end,
            shutdown = function() self.running = false end,
            reboot = function() self.running = 2 end,
            getComputerID = function() return self.id end,
            getComputerLabel = function() return self.label end,
            setComputerLabel = function(label)
                expect(1, label, "string", "nil")
                self.label = label
            end,
            clock = function() return os.clock() - self.startTime end,
            time = os.time,
            day = os.day,
            epoch = os.epoch,
            date = os.date,
        },
        peripheral = {
            isPresent = function(side)
                expect(1, side, "string")
                return self.peripherals[side] ~= nil
            end,
            getType = function(side)
                expect(1, side, "string")
                if self.peripherals[side] then return peripheral.getType(self.peripherals[side]) end
                return nil
            end,
            hasType = function(side, type)
                expect(1, side, "string")
                expect(2, type, "string")
                return self.peripherals[side] and peripheral.hasType(self.peripherals[side], type) or nil
            end,
            getMethods = function(side)
                expect(1, side, "string")
                return self.peripherals[side] and peripheral.getMethods(self.peripherals[side]) or nil
            end,
            call = function(side, method, ...)
                expect(1, side, "string")
                expect(2, method, "string")
                if self.peripherals[side] then return peripheral.call(self.peripherals[side], method, ...) end
                error("No such peripheral", 2)
            end,
        },
        redstone = {
            getSides = function() return orangebox.sideNumbers end,
            getInput = function(side)
                expect(1, side, "string")
                if orangebox.sideNames[side] == nil then error("bad argument #1 (invalid option " .. side .. ")", 2) end
                return self.redstone.input[orangebox.sideNames[side]] ~= 0
            end,
            getOutput = function(side)
                expect(1, side, "string")
                if orangebox.sideNames[side] == nil then error("bad argument #1 (invalid option " .. side .. ")", 2) end
                return self.redstone.output[orangebox.sideNames[side]] ~= 0
            end,
            setOutput = function(side, output)
                expect(1, side, "string")
                expect(2, output, "boolean")
                if orangebox.sideNames[side] == nil then error("bad argument #1 (invalid option " .. side .. ")", 2) end
                self.redstone.output[orangebox.sideNames[side]] = output and 15 or 0
            end,
            getAnalogInput = function(side)
                expect(1, side, "string")
                if orangebox.sideNames[side] == nil then error("bad argument #1 (invalid option " .. side .. ")", 2) end
                return self.redstone.input[orangebox.sideNames[side]]
            end,
            getAnalogOutput = function(side)
                expect(1, side, "string")
                if orangebox.sideNames[side] == nil then error("bad argument #1 (invalid option " .. side .. ")", 2) end
                return self.redstone.output[orangebox.sideNames[side]]
            end,
            setAnalogOutput = function(side, output)
                expect(1, side, "string")
                expect(2, output, "number")
                if orangebox.sideNames[side] == nil then error("bad argument #1 (invalid option " .. side .. ")", 2) end
                if output < 0 or output > 15 then error("Expected number in range 0-15", 2) end
                self.redstone.output[orangebox.sideNames[side]] = output
            end,
            getBundledInput = function(side)
                expect(1, side, "string")
                if orangebox.sideNames[side] == nil then error("bad argument #1 (invalid option " .. side .. ")", 2) end
                return self.redstone.bundledInput[orangebox.sideNames[side]]
            end,
            getBundledOutput = function(side)
                expect(1, side, "string")
                if orangebox.sideNames[side] == nil then error("bad argument #1 (invalid option " .. side .. ")", 2) end
                return self.redstone.bundledOutput[orangebox.sideNames[side]]
            end,
            setBundledOutput = function(side, output)
                expect(1, side, "string")
                expect(2, output, "number")
                if orangebox.sideNames[side] == nil then error("bad argument #1 (invalid option " .. side .. ")", 2) end
                if output < 0 or output > 65535 then error("Expected number in range 0-65535", 2) end
                self.redstone.bundledOutput[orangebox.sideNames[side]] = output
            end,
            testBundledInput = function(side, mask)
                expect(1, side, "string")
                expect(2, mask, "number")
                if orangebox.sideNames[side] == nil then error("bad argument #1 (invalid option " .. side .. ")", 2) end
                if mask < 0 or mask > 65535 then error("Expected number in range 0-65535", 2) end
                return bit32.btest(self.redstone.bundledInput[orangebox.sideNames[side]], mask)
            end
        }
    }
    env._G = env
    env.os.computerID = env.os.getComputerID
    env.os.computerLabel = env.os.getComputerLabel
    env.rs = redstone
    env.redstone.getAnalogueInput = env.redstone.getAnalogInput
    env.redstone.getAnalogueOutput = env.redstone.getAnalogOutput
    env.redstone.setAnalogueOutput = env.redstone.setAnalogOutput
    for k, v in pairs(self.apis) do env[k] = v end
    return env
end

--- Returns the global environment for the box.
-- @treturn table The box's global environment.
function orangebox:getfenv()
    return getfenv(self.fn)
end

--- Loads the BIOS if necessary and sets a new environment.
function orangebox:reloadenv()
    if self.bios == nil then error("No BIOS was specified. Please set self.bios to the contents of the BIOS script.", 2) end
    if self.fn == nil then self.fn = load(self.bios, "=bios.lua", "t", self:makeenv())
    else setfenv(self.fn, self:makeenv()) end
end

--- Resumes the box's execution, or starts it if it's not running.
function orangebox:resume()
    local ok
    repeat
        if not self.coro then
            if self.bios == nil then error("No BIOS was specified. Please set self.bios to the contents of the BIOS script.", 2) end
            if self.fn == nil then self.fn = load(self.bios, "=bios.lua", "t", self:makeenv()) end
            self.coro = coroutine.create(self.fn)
        end
        if self.running ~= true then
            self.running = true
            self.startTime = os.clock()
            ok, self.filter = coroutine.resume(self.coro)
            if not ok then
                local err = self.filter
                self.filter = nil
                self.coro = nil
                self.running = false
                self.startTime = nil
                self.eventQueue = {}
                self:syncfs(true)
                error("orangebox environment threw an exception: " .. err, 2)
            end
        end
        while #self.eventQueue > 0 and self.running == true do
            local ev = table.remove(self.eventQueue, 1)
            if self.filter == nil or self.filter == ev[1] or ev[1] == "terminate" then
                if ev[1] == "timer" then self.timers[ev[2]] = nil
                elseif ev[1] == "alarm" then self.alarms[ev[2]] = nil end
                if  ((self.sync_last + self.sync_cooldown_ms) >= os.epoch('utc')) and self.compression_enabled and self.fs_dirty then self:syncfs() end
                ok, self.filter = coroutine.resume(self.coro, table.unpack(ev))
                if not ok then
                    local err = self.filter
                    self.filter = nil
                    self.coro = nil
                    self.running = false
                    self.startTime = nil
                    self.eventQueue = {}
                    self:syncfs(true)
                    error("orangebox environment threw an exception: " .. err, 2)
                end
            end
        end
        if self.running ~= true then
            self.filter = nil
            self.coro = nil
            self.startTime = nil
            self.eventQueue = {}
            self:syncfs(true)
        end
    until self.running ~= 2
end

--- Queues an event inside the box.
-- @tparam string event The event name to queue.
-- @param ... The event's arguments.
function orangebox:queueEvent(event, ...)
    expect(1, event, "string")
    if event == "timer" and self.timers[...] == nil or event == "alarm" and self.alarms[...] == nil then return end
    self.eventQueue[#self.eventQueue + 1] = {event, ...}
end

--- Halts the box, deleting any coroutines and resetting the state.
function orangebox:halt()
    self.running = false
    self.coro = nil
    self.startTime = nil
    self.eventQueue = {}
    self:syncfs()
end

--- Mounts a path from outside the box inside the environment.
-- @tparam string innerPath The path to the mount inside the box.
-- @tparam string outerPath The path to the mounted folder outside the box.
function orangebox:mount(innerPath, outerPath)
    expect(1, innerPath, "string")
    expect(2, outerPath, "string")
    local parts = {}
    for p in fs.combine(innerPath):gmatch("[^/]+") do parts[#parts + 1] = p end
    self.mounts[parts] = fs.combine(outerPath)
end

--- Unmounts a previously mounted directory.
-- @tparam string innerPath The path to the mount inside the box.
function orangebox:unmount(innerPath)
    expect(1, innerPath, "string")
    local parts = {}
    for p in fs.combine(innerPath):gmatch("[^/]+") do parts[#parts + 1] = p end
    for k in pairs(self.mounts) do
        if #k == #p and table.concat(k, "/") == table.concat(p, "/") then
            self.mounts[k] = nil
            break
        end
    end
end

--- Loads a BIOS file from a path.
-- @tparam string path The path to the BIOS.
function orangebox:loadBIOS(path)
    expect(1, path, "string")
    local file, err = fs.open(path, "rb")
    if file == nil then error(err, 2) end
    self.bios = file.readAll()
    file.close()
end

if found_libdeflate then
    --- sets the status of compression for disk files
    -- this uses LibDeflate [LibDeflate](https://github.com/MCJack123/CC-Archive/#libdeflate)
    -- this option only appears if LibDeflate was able to be required via `require("LibDeflate")`
    -- @tparam boolean enable whether or not to compress vfs files
    function orangebox:enableCompression(enable)
        self.compression_enabled = enable
    end
    --- gets the status of compression for disk files
    -- this option only appears if LibDeflate was able to be required via `require("LibDeflate")`
    -- @treturn boolean whether or not compression is enabled
    function orangebox:compressionEnabled()
        return self.compression_enabled
    end
else
    --print("Unnable to load LibDeflate:")
    --print(libdeflate)
end

--- Loads a VFS disk and optionally sets up write-backs.
-- This uses a basic serialized table to store data. More efficient methods are
-- available, but these are left to the user to implement.
-- @tparam string path The path to the VFS.
-- @tparam boolean|nil readOnly Whether the disk is read-only (won't sync changes to the file).
function orangebox:loadVFS(path, readOnly)
    expect(1, path, "string")
    expect(2, readOnly, "boolean", "nil")
    local file = fs.open(path, "rb")
    if file ~= nil then
        local data = file.readAll()
        file.close()
        if self.compression_enabled then
            self.disk = textutils.unserialize(libdeflate:DecompressGzip(data))
        else
            self.disk = textutils.unserialize(data)
        end
    else self.disk = {} end
    if not readOnly then
        self.syncfs = function(self,force)
            if ((self.sync_last + self.sync_cooldown_ms) >= os.epoch("utc")) and self.compression_enabled or force then
                local file = fs.open(path, "wb")
                if file == nil then return end
                if self.compression_enabled then
                    file.write(libdeflate:CompressGzip(textutils.serialize(self.disk, {compact = true})))
                else
                    file.write(textutils.serialize(self.disk, {compact = true}))
                end
                file.close()
                self.fs_dirty = false
                self.sync_last = os.epoch("utc")
            else
                self.fs_dirty = true
            end
        end
    else self.syncfs = function() end end
end

--- Exports a peripheral from outside the box in.
-- @tparam string name The name of the peripheral as available outside the box.
-- @tparam string|nil innerName The name of the peripheral as it will appear inside the box. Defaults to the same name as outside.
function orangebox:exportPeripheral(name, innerName)
    expect(1, name, "string")
    expect(2, innerName, "string", "nil")
    self.peripherals[innerName or name] = name
end

return orangebox
