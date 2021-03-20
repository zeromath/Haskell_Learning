def find_quotient(d):
    r = 10
    r_list = []
    while (r != 0):
        if d <= r:
            r %= d
        if r in r_list:
            break
        r_list.append(r)
        r *= 10

    if r_list[-1] == 0:
        return 0
    else:
        return len(r_list)

if __name__ == '__main__':
    max_recurrence = -1
    max_number = 0
    
    for i in range(998):
        r = find_quotient(i+2)
        if r>max_recurrence:
            max_recurrence = r
            max_number = i + 2
    print(max_number) # 983
