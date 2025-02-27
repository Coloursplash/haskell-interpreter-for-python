def firstLastSame(numberList):
    print("Given list:", numberList)
    
    first = numberList[0]
    last = numberList[-1]
    print("First: " + str(first) + "   Last: " + str(last))
    if first == last:
        return True
    else:
        return False

nums1 = [10, 20, 30, 40, 10]
print("result is", firstLastSame(nums1))

nums2 = [75, 65, 35, 75, 30]
print("result is", firstLastSame(nums2))