This is a replication package for ICSE2020 submission.

The structure of the folder follows the paper structure. 

1. data
We put our subject data in the folder named data. In detail, there are three sub-folders inside, including metrics, perfBugData, perRawData. 
1.1 metrics
(1) commit-impactedtest-changes.txt: The studied commits and the impacted test and covered code changes.
(2) hadoop.csv, cassandra.csv, and openjpa.csv: Traditional metrics, performance-related metrics we extracted.
1.2 perBugData
This folder includes the performance bug issue data.
(1) bug-inducing-changes.csv: Issues from JIRA report contain issue fixing commit, issue inducing commit.
(2) perf-bug-commit-issue.csv: Performance issues filtered.
1.3 perfRawData
This folder includes the raw performance data collected from Microsoft Azure.
(1) responsetime.out: response time data
(2) performance.out: performance counter data, including CPU, Memory, and Disk IO data.

2. code
We put our source code in this folder. 
2.1 preliminary
This sub-folder contains the source code to do preliminary analysis.
2.2 rq1-4
Each folder rqX corresponds to the research question of the paper.
2.3 lib
This folder includes the packages needed, the model building code.

3. result
In this folder, we put our results corresponding to the research question. You can find the detail of the results in these folders. 
For example, due to the limited space, we only present AUC of all classifiers while presenting Random forest with precision and recall. You can find the detail results in the detailResult.pdf in the sub-folder rq1.