import os

def delete_multiple_lines(original_file, line_numbers):
    """In a file, delete the lines at line number in given list"""
    is_skipped = False
    counter = 0
    # Create name of dummy / temporary file
    dummy_file = os.path.join(os.getcwd(), "data", "census", "clean", original_file)
    # Open original file in read only mode and dummy file in write mode
    with open(original_file, 'r') as read_obj, open(dummy_file, 'w') as write_obj:
        # Line by line copy data from original file to dummy file
        for line in read_obj:
            # If current line number exist in list then skip copying that line
            if counter not in line_numbers:
                write_obj.write(line)
            else:
                is_skipped = True
            counter += 1

for f in os.listdir(os.path.join(os.getcwd(), "data", "census")):
    if f[-7:] == "all.csv" or f[-9:] == "Large.csv":
        delete_multiple_lines(os.path.join(os.getcwd(), "data", "census", f), [i for i in range(243, 333)])
