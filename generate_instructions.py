instructions = []

with open("instructions.txt", "r") as instructions_file:
    for line in instructions_file:
        tokens = line.strip().split(",")
        if len(tokens) != 2:
            print(tokens)
            assert False
        opcode = tokens[0].strip()
        label = tokens[1].strip()
        #print(opcode, label)
        instructions += [(opcode, label)]

# instructions.sort(key=lambda x: int(x[0][:16].replace("x", "0"), 2))
# # instructions.sort(key=lambda x: x[0])
# for i in instructions:
#     print(i)

# template1 = '''const {0}_MSK: u16 = 0b{1};
# const {0}_VAL: u16 = 0b{2};
# fn {3}(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {{}}'''

# template2 = '''}} else if (instruction0 & {0}_MSK) == {0}_VAL {{
# {1}(instruction0, &mut cpu, &mut memory);'''
# definitions = ""
# calls = ""
# for opcode, label in instructions:
#     # opcode = opcode[:16]
#     opcode = opcode[:4]+"_"+opcode[4:8]+"_"+opcode[8:12]+"_"+opcode[12:]
#     label = label.replace(" ", "_")
#     val = opcode.replace('x', '0')
#     msk = opcode.replace('0', '1').replace('x', '0')
#     label_upper = label.upper()
#     definitions += template1.format(label_upper, msk, val, label) + "\n"
#     calls += template2.format(label_upper, label) + "\n"

# print(definitions)
# print(calls)

jump_table_details = []


jump_table_bit_width = 8
fmt_str = r"{0} (0b{0:0" + str(jump_table_bit_width) + r"b}):"
for jump_table_index in range(1 << jump_table_bit_width):
    print(fmt_str.format(jump_table_index))
    for opcode, label in instructions:
        bits = opcode[:jump_table_bit_width]
        mask = int(bits.replace('0', '1').replace('x', '0'), 2)
        value = int(bits.replace('x', '0'), 2)

        if jump_table_index & mask == value:
            print("\t{} {}".format(opcode, label))
            jt_range = (jump_table_index, jump_table_index+1)
            jt_ops = set((label,))
            jump_table_details += [(jt_range, jt_ops)]

print("\n\n")
for jt in jump_table_details:
    print(jt)



i = 0
while i+1 < len(jump_table_details):
    if jump_table_details[i][0][0] == jump_table_details[i+1][0][0]:
        range_start = jump_table_details[i][0][0]
        range_end = jump_table_details[i+1][0][1]
        new_range = (range_start, range_end)
        new_ops = jump_table_details[i][1] | jump_table_details[i+1][1]
        jump_table_details[i] = (new_range, new_ops)
        jump_table_details.pop(i+1)
    else:
        i += 1


print("\n\n")
for jt in jump_table_details:
    print(jt)

i = 0
while i+1 < len(jump_table_details):
    first_ops = jump_table_details[i][1]
    second_ops = jump_table_details[i+1][1]
    if first_ops.isdisjoint(second_ops):
        i += 1
    else:
        range_start = jump_table_details[i][0][0]
        range_end = jump_table_details[i+1][0][1]
        new_range = (range_start, range_end)
        new_ops = jump_table_details[i][1] | jump_table_details[i+1][1]
        jump_table_details[i] = (new_range, new_ops)
        jump_table_details.pop(i+1)

    # if first_ops == second_ops:
    #     range_start = jump_table_details[i][0][0]
    #     range_end = jump_table_details[i+1][0][1]
    #     new_range = (range_start, range_end)
    #     new_ops = jump_table_details[i][1] | jump_table_details[i+1][1]
    #     jump_table_details[i] = (new_range, new_ops)
    #     jump_table_details.pop(i+1)
    # else:
    #     i += 1

print("\n\n")
for jt in jump_table_details:
    instr = list(jt[1])
    print("    0b{:08b}..=0b{:08b} => ".format(jt[0][0], jt[0][1]-1), end="")
    if len(instr) == 1:
        instruction = '_'.join(instr[0].split(" "))
        print("instructions::{}(instruction0, &mut cpu),".format(instruction))
    elif len(instr) > 1:
        print("{")
        for i in instr:
            instruction = '_'.join(i.split(" "))
            mask = instruction.upper() + "_MSK"
            value = instruction.upper() + "_VAL"
            print("        else if (instruction0 & instructions::{}) == instructions::{} {{instructions::{}(instruction0, &mut cpu)}}".format(mask, value, instruction))
        print("        else {panic!()}")
        print("    }")
    # print(jt)