def fib(n):
    if n > 2:
        return fib(n-1) + fib(n-2)
    else:
        return 1

num = int(input("Which fibonacci number would you like to calculate: "))
print("The " + str(num) + "th fibonacci number is: ",fib(num))