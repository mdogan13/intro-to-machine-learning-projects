import csv
import numpy as np
import matplotlib.pyplot as plt
# COMP421 HW05
# Murat Dogan
# 27624

f = "hw05_data_set.csv"

data = csv.reader(open(f))

li = []
index = 0
for line in data:
    a = line
    a.append(index)
    li.append(tuple(a))
    index = index+1

dataset = li[1:]

trainset = dataset[0:100]
testset = dataset[100:133]

trainx=[t[0] for t in trainset]
trainy=[t[1] for t in trainset]

testx=[t[0] for t in testset]
testy=[t[1] for t in testset]

terminal_nodes = []
split_positions = []
P = 10

def floatize(list):
    new_list = []
    for item in list:
        new_list.append(float(item))

    return new_list


def tree(dataset):
    if len(dataset)<P:
        # no need to split, given input is a terminal node
        terminal_nodes.append(dataset)
    else:
        # will split on the best position
        SSE = []

        for i in range(len(dataset)):
            # split by split value
            splitval = dataset[i][1]
            leftsplit = [x for x in dataset if float(x[1]) <= float(splitval)]
            rightsplit =[x for x in dataset if float(x[1]) > float(splitval)]

            # grab y values
            lefty =[x[1] for x in leftsplit]
            righty = [x[1] for x in rightsplit]
            new_lefty = floatize(lefty)
            new_righty = floatize(righty)

            # calculate means
            leftmean= np.mean(new_lefty)
            rightmean = np.mean(new_righty)

            # calulate SSE
            new_lefty[:] = [x - leftmean for x in new_lefty]
            new_righty[:] = [x - rightmean for x in new_righty]
            SSE.append(sum(new_lefty)*sum(new_lefty)+sum(new_righty)*sum(new_righty))

        # create children
        split_positions.append(np.argmin(SSE))
        left_child = dataset[0:np.argmin(SSE)-1]
        right_child = dataset[np.argmin(SSE)-1:]

        # call the algorithm on children
        tree(left_child)
        tree(right_child)

tree(trainset)

# calculating mean values for each terminal node
terminal_nodes_means = []
for list in terminal_nodes:
    list_y = [t[1] for t in list]
    list_y_fl = floatize(list_y)
    list_y_mean = np.mean(list_y_fl)
    terminal_nodes_means.append(list_y_mean)

# print(terminal_nodes_means)
# nodesizes = [len(list) for list in terminal_nodes]
# print(nodesizes)

print("Split positions:")
print(split_positions)
print("Terminal nodes:")
for node in terminal_nodes:
    print(node)


# each data point's terminal node mean
extended_means = [-15.1,-15.1,-15.1,-15.1,-15.1,-15.1,-15.1,10.459999999999999,10.459999999999999,10.459999999999999,10.459999999999999,10.459999999999999,-33.739999999999995,-33.739999999999995,-33.739999999999995,-33.739999999999995,-33.739999999999995,8.6999999999999993,8.6999999999999993,-54.600000000000009,-54.600000000000009,-54.600000000000009,-54.600000000000009,-7.8142857142857149,-7.8142857142857149,-7.8142857142857149,-7.8142857142857149,-7.8142857142857149,-7.8142857142857149,-7.8142857142857149,7.3499999999999996,7.3499999999999996,-44.166666666666664,-44.166666666666664,-44.166666666666664,-44.166666666666664,-44.166666666666664,-44.166666666666664,-44.166666666666664,-44.166666666666664,-44.166666666666664,-59.25,-59.25,-59.25,-59.25,-35.939999999999998,-35.939999999999998,-35.939999999999998,-35.939999999999998,-35.939999999999998,-19.300000000000001,-19.300000000000001,-19.300000000000001,-19.300000000000001,-19.300000000000001,-42.899999999999999,-2.7000000000000002,-43.600000000000001,-43.600000000000001,-43.600000000000001,-43.600000000000001,-43.600000000000001,-43.600000000000001,-43.600000000000001,8.375,8.375,8.375,8.375,-22.549999999999997,-22.549999999999997,-22.549999999999997,-22.549999999999997,-22.549999999999997,-22.549999999999997, -8.6500000000000004, -8.6500000000000004,-35.300000000000004,-35.300000000000004,-35.300000000000004,-33.755555555555553,-33.755555555555553,-33.755555555555553,-33.755555555555553,-33.755555555555553,-33.755555555555553,-33.755555555555553,-33.755555555555553,-33.755555555555553,-16.0,-20.399999999999999,-20.399999999999999,-20.399999999999999,-20.399999999999999,-7.4857142857142849,-7.4857142857142849,-7.4857142857142849,-7.4857142857142849,-7.4857142857142849,-7.4857142857142849,-7.4857142857142849]

plt.plot(trainx, trainy, 'bo', label='training')
plt.plot(testx, testy, 'ro',label='test')
plt.plot(trainx, extended_means, 'co', label='terminal node means')
plt.xlabel('x')
plt.ylabel('y')
plt.title('P = 10')
plt.legend(loc=4)
plt.show()

