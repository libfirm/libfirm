print("Processing IR templates...")

local path_root = path.join(os.getcwd(), "../")

local path_input = path.join(path_root, "scripts", "templates")
local path_output = path.join(path_root, dst, gen)

os.mkdir(path_output)

local templates = {
    "nodes.h",
    "gen_proj_names.h",
    "gen_irnode.h",
    "gen_irnode.c",
    "gen_irio.c"
}

local script_ir_gen = path.join(path_root, "scripts", "gen_ir.py")
local script_ir_spec = path.join(path_root, "scripts", "ir_spec.py")

for _, template in ipairs(templates) do

    local input = path.join(path_input, template)
    local output = path.join(path_output, template)

    local command = {
        "python",
        script_ir_gen,
        script_ir_spec,
        input,
        ">",
        output
    }

    os.execute(table.concat(command, " "))
end
