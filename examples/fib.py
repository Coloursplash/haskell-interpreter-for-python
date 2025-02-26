def fib(n):
    out = 1
    if n > 2:
        out = fib(n-1) + fib(n-2)
        
    return out 
    
num = int(input("Which fibonacci number would you like to calculate: "))
print("The " + str(num) + "th fibonacci number is: ",fib(num))