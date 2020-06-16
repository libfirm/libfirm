print("Generating back ends...")

local path_root = path.join(os.getcwd(), "../")
local path_ir_be = path.join(path_root, "ir", "be")
local path_scripts = path.join(path_ir_be, "scripts")

local path_output = path.join(path_root, dst, gen)

function backend_generator(action, arch)

    local script = path.join(path_scripts, "generate_" .. action .. ".pl")
    local spec = path.join(path_ir_be, arch, arch .. "_spec.pl")

    local command = {
        "perl",
        script,
        spec,
        path_output
    }

    os.execute(table.concat(command, " "))
end

local actions = {
    "emitter",
    "regalloc_if",
    "new_opcodes"
}

local archs = {
    "ia32",
    "arm",
    "amd64",
    "TEMPLATE"
}

for _, arch in ipairs(archs) do

    print("Generating " .. arch .. "...")

    for _, action in ipairs(actions) do
        backend_generator(action, arch)
    end
end
