def qsort(num_list):
    if num_list == []: return []
    return qsort(list(filter(lambda x: x <= num_list[0], num_list[1:]))) + num_list[0:1] + qsort(list(filter(lambda x: x > num_list[0], num_list[1:])))

print(qsort([1, 3, 4, 32, 23, 45, 6, 2, 1, 436, 5, 7]))
