# xml-comparator

Currently having an issue with Excel interacting with Atscale where in one project data is correctly displayed while
another project data is not for Excel's "cubevalue" function. We have xml dump from both projects and would like to
compare the below:

1. identify meta-data files (both requests and responses) within a single project, group them as per their meta-data
   request and compare files within a group for matching or not.
1. if (1) matches, then group meta-data files as per (project, meta-data) criteria and them compare similar meta-data
   requests across projects to identify the differences.

next step:
test end to end now for a discoverResponse file