import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

import javax.lang.model.util.ElementScanner6;

class Node {
    private int nofChildren;
    private int nofMetas;
    private ArrayList<Node> children;
    private ArrayList<Integer> metas;
    Node (Scanner sc) {
        nofChildren = sc.nextInt();
        nofMetas = sc.nextInt();
        children = new ArrayList<Node>();
        metas = new ArrayList<Integer>();
        for (int i=0; i<nofChildren; ++i)
            children.add(new Node(sc));
        for (int i=0; i<nofMetas; ++i)
            metas.add(sc.nextInt());
    }
    int calcMetas() {
        int res = 0;
        for (int i : metas) 
            res += i;
        for (Node n : children)
            res += n.calcMetas();
        return res;
    }
    int calcValue() {
        int res = 0;
        if (nofChildren == 0)
            for (int i : metas)
                res += i;
        else
            for (int i : metas)
                if (0 < i && i <= nofChildren)
                    res += children.get(i-1).calcValue();
        return res;
    }
}

class day08 {
    private static Scanner getIntScanner(String fileName) throws FileNotFoundException {
        return new Scanner(new File(fileName));
    }
    public static void main(String args[]) throws FileNotFoundException {
        Scanner s = getIntScanner("day08.txt");
        Node n = new Node(s);
        System.out.printf("Solution for day 8 part 1: %d\n", n.calcMetas());
        System.out.printf("Solution for day 8 part 2: %d\n", n.calcValue());
    }
}