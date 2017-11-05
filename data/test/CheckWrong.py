





def testForErrors(filename):
    f = open(filename);
    f.readline();
    f.readline();
    lines = f.readlines();
    edges = [];

    for line in lines:
        parts = line.split();
        if(len(parts) >= 2):
            e = (parts[0], parts[1]) 
            if e in edges:
                print "EDGE", e, "APPEARS MULTIPLE TIMES in ", filename
            edges.append(e);
            edges.append( (parts[1], parts[0]) )

for i in range(1,11):
    s = "TestFile" + str(i) + ".uwg";
    testForErrors(s);