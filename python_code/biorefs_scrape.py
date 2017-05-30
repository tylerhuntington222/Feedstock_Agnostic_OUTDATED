# Read in the raw html from the map of biorefineries at:
# http://www.ethanolrfa.org/resources/biorefinery-locations/
# and extract names and addresses of all refineries.

import csv

def main ():
    # set file name
    filename = "biorefineries_raw_html.txt"

    # read in file
    infile = open(filename, "r")

    data = infile.readlines()
    #print(data)
    print(data[0])
    print(data[1])
    print(data[2])

    for i in range(len(data)):
        # init blank list
        L = []

        name = GetName(data[i])
        print name
        add = GetAddress(data[i])
        L.append(name)
        L.append(add)
        data[i] = L

    #print (data)
    print (data)

    with open("../data/biorefs_output.csv", "wb") as f:
        writer = csv.writer(f)
        writer.writerows(data)


def GetName(profile):
    '''
    PURPOSE: get the name of a refinery from its raw html mapid format
    PARAMS:
        profile - html encoded profile of refinery a string of chars
    RETURNS: name of refinery as string
    '''

    # iterate over chars in profile
    for i in range(len(profile)):
        if (profile[i] == '"'):
            if (profile[i+1] == ","):
                break
    # slice profile for refinery name only
    name = profile[1:i]

    return name

def GetAddress(profile):
    '''
    PURPOSE: get the address of a refinery from its raw html mapid format
    PARAMS:
        profile - html encoded profile of refinery a string of chars
    RETURNS: address of refinery as string
    '''

    # iterate over chars in profile
    for i in range(len(profile)):
        if (profile[i] == "a"):
            if (profile[i+1] == "d"):
                if (profile[i+2] == "d"):
                    if (profile[i+3] == "r"):
                        if (profile[i+4] == "e"):
                            break

    # slice profile for refinery name and trailing text
    add_plus = profile[i+10:]
    add = AddClip(add_plus)
    return add

def AddClip(add_plus):

    # find the end of the address in profile string
    for j in range(len(add_plus)):
        if (add_plus[j] == ","):
            if (add_plus[j+1] == '"'):
                if (add_plus[j+2] == "d"):
                    if (add_plus[j+3] == "e"):
                        if (add_plus[j+4] == "s"):
                            if (add_plus[j+5] == "c"):
                                break

    # slice profile for refinery name only
    add = add_plus[:j-1]

    return add


main()
