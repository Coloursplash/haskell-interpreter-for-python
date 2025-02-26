print("Printing current and previous number and their sum in a range(10)")
prev = 0

# loop from 1 to 10
for i in range(1, 11):
    total = prev + i
    print("Current Number:", i, "  Previous Number:", prev, "  Sum:", total)
    # modify previous number
    # set it to the current number
    prev = i
