print("hello from default obazl_watch.lua (src/lua/obazl_watch.lua)")

bazel["init"] = function()
      print("LUA init (tools_obazl)")
      dd = require("datadumper")

      -- print("bazel table:")
      -- print(DataDumper(bazel))
      for pname, pkg in pairs(bazel.packages) do
         -- print("")
         -- print("pname : " .. pname)
         for modname, mtbl in pairs(pkg.modules) do
            -- print("modname: " .. modname);
            if mtbl.structfile then
               local deps = mtbl.structfile.deps
               if deps then
                  mtbl.structfile.deps["targets"] = update_deps(deps)
               end
            end -- structfile
            if mtbl.sigfile then
               local deps = mtbl.sigfile.deps
               if deps then
                  mtbl.sigfile.deps["targets"] = update_deps(deps)
               end
            end -- structfile
         end
      end
      print(DataDumper(bazel.packages))
      print(DataDumper(bazel.modules))
end

bazel["on_fs_event"] = function(ev)
   print("default obazl_watch.lua:on_fs_event")
   print("ev keys: ")
   for k,_ in pairs(ev) do
      print(" ", k)
   end
   print("ev.path: " .. ev.path)
   print("ev.time: " .. ev.time)
   print("flags:")
   for k,_ in pairs(ev.flags) do
      print(" ", k)
   end
   if ev.flags.Updated then
      print("Updated " .. ev.path)
   end
end

function update_deps(the_deps)
   -- print("update_mdeps")
   local targets = {}
   for _, tmdep in ipairs(the_deps.modules) do
      module_dep = tmdep.dep
      -- print(" ", "looking up " .. module_dep)
      -- now lookup module_dep in targets table
      m = bazel.modules[module_dep]
      if m then
         -- print(" ", "found: " .. m)
         local already = false
         for k,v in pairs(targets) do
            -- print(" ", k, v.dep)
            if v.dep == m then
               already = true
               break
            end
         end
         if already == false then
            table.insert(targets, {dep = m }) -- "//foo/" .. module_dep})
         end
      end
   end
   return targets
end

function emit_build_lua(module)
   print("emit_build_lua entry, module: " .. module)
   print("bazel.packages:")
   for k,v in pairs(bazel.packages) do
      print(k,v)
      for kk,vv in pairs(v) do
         print("  ", kk,vv)
      end
      print("modules:")
      for km,vm in pairs(v.modules) do
         print("== ", km, vm)
         for kkm,vvm in pairs(vm) do
            print("\t  ", kkm, vvm)
         end
      end
   end
   return "ok"
end

   -- print("LUA MODULES:\n",(package.path:gsub("%;","\n\t")),"\n\nC MODULES:\n",(package.cpath:gsub("%;","\n\t")))
   -- print("package.path: ")
   -- print(package.path)
