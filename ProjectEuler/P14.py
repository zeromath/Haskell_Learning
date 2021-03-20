data_size = 1000000
list_of_length = [0 for _ in range(data_size)]
list_of_length[0] = 1

def callCollatzLength(n):
    if n > data_size:
        if n % 2 == 0:
            return 1 + callCollatzLength(n // 2)
        else:
            return 1 + callCollatzLength(3 * n + 1)
    elif list_of_length[n - 1] == 0:
        if n % 2 == 0:
            list_of_length[n - 1] = 1 + callCollatzLength(n // 2)
        else:
            list_of_length[n - 1] = 1 + callCollatzLength(3 * n + 1)
    return list_of_length[n - 1]

max_value = -1
max_key = 0

for k in range(data_size):
    v = callCollatzLength(k + 1)
    if v > max_value:
        max_value = v
        max_key = k

print(max_key + 1) # 837799

