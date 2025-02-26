x = int(input('Please enter a number: '))    # this should be int(input( ... )), but havent implemented the int function

if x == 4:
    print(x, "equals 4!")

if x % 3 == 0 and x % 2 == 0:
    print(x, "is a multiple of 2 and 3")
elif x % 3 == 0:
    print(x, "is a multiple of 3")
elif x % 2 == 0:
    print(x, "is a multiple of 2")
else:
    print(x, "is not a multiple of 2 or 3")