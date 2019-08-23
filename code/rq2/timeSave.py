import numpy as np


# calculate the time each test running
config="./config/"
runtimeFilename= config + "runtime.csv"
#open true value and predict value
predictFilename=config+"cassandraPredict.txt"
#open the commit index from 51th commit to the last commit
commitindexFilename=config+"commit_index.txt"
def runtimeofTest():
    runtimeFile=open(runtimeFilename)
    runtimeList=runtimeFile.readlines()
    runtimes = []
    for index,test in enumerate(runtimeList):
        if index%2 == 0:
            curtimes=runtimeList[index].split(',')
            partimes=runtimeList[index+1].split(',')
            curtimes=map(eval,curtimes)
            partimes=map(eval,partimes)
            averageTime=sum(curtimes)/30 + sum(partimes)/30
            # newRuntimeFile.write(str(averageTime)+'\n')
            runtimes.append(averageTime)
    timePercent(runtimes)

def timePercent(runtime_list):
    predictFile=open(predictFilename)
    predictList=predictFile.readlines()
    #get the commit index
    commitIndexFile=open(commitindexFilename)
    commitIndexList=commitIndexFile.readlines()
    commitIndexs = commitIndexList[0][:-1].split(' ')
    commitIndexs = map(eval,commitIndexs)
    commitIndexs = [x-183 for x in commitIndexs]

    for index,true_value in enumerate(predictList):#iterate the runtime,cpu,memory and io
        tp_total_time = 0
        tn_total_time = 0
        fp_total_time = 0
        fn_total_time = 0

        if index%2 == 0:
            true_list = predictList[index][0:-1].split(' ')
            predict_list = predictList[index+1][:-1].split(' ')
            # true_list = map(eval,true_list)
            # predict_list = map(eval,predict_list)
            lengthList = [x for x in range(len(true_list))]
            for c_index,true_v,predict_v,runtime in zip(lengthList,true_list,predict_list,runtime_list):
                if(true_v=='1' and predict_v=='1'):
                    tp_total_time += runtime
                elif(true_v=='1' and predict_v=='0'):
                    tn_total_time += runtime
                elif(true_v=='0' and predict_v=='1'):
                    fp_total_time += runtime
                else:
                    fn_total_time += runtime
            print int(tp_total_time),'\t',int(tn_total_time),'\t',int(fp_total_time),'\t',int(fn_total_time)
    # dataSet = np.loadtxt(predictFilename,delimiter=" ")
    # print dataSet.T

runtimeofTest()
