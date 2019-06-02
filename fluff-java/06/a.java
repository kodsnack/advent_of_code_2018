import java.awt.*;
import java.util.*;
import java.util.List;
import java.util.stream.Collectors;

class Day06A {

    public static void main(String[] args) {

        HashMap<Integer,Point> points = new HashMap<>();
        HashMap<Point,Integer> mapOwners = new HashMap<>();
        Point min = new Point();
        Point max = new Point();

        Scanner in = new Scanner(System.in);
        while(in.hasNext()) {
            String[] str = in.nextLine().split(", ");
            Point p = new Point(Integer.parseInt(str[0]), Integer.parseInt(str[1]));
            points.put(points.size(), p);
            if(points.size() == 1) {
                min = new Point(p);
                max = new Point(p);
            }

            if(min.x > p.x) {
                min.setLocation(p.x, min. y);
            }
            if(min.y > p.y) {
                min.setLocation(min.x, p.y);
            }
            if(max.x < p.x) {
                max.setLocation(p.x, max.y);
            }
            if(max.y < p.y) {
                max.setLocation(max.x, p.y);
            }
        }

        in.close();

        for(int x = 0; x <= max.x; x++) {
            for(int y = 0; y <= max.y; y++) {
                HashMap<Integer,Integer> ownerDistances = new HashMap<>();
                for(Map.Entry<Integer,Point> e : points.entrySet()) {
                    ownerDistances.put(e.getKey(),Math.abs(x - e.getValue().x) + Math.abs(y - e.getValue().y));
                }

                int mindist = ownerDistances.entrySet().stream().min(Map.Entry.comparingByValue()).get().getValue();
                List<Integer> owners = ownerDistances.entrySet().stream().filter(e -> e.getValue() == mindist).map(e -> e.getKey()).collect(Collectors.toList());
                if(((List) owners).size() == 1) {
                    mapOwners.put(new Point(x, y), owners.get(0));
                }
                else {
                    mapOwners.put(new Point(x, y), -1);
                }
            }
        }

        HashMap<Integer,Integer> totalCount = new HashMap<>();
        for(Map.Entry<Point, Integer> e : mapOwners.entrySet()) {

            if(totalCount.containsKey(e.getValue())) {
                totalCount.put(e.getValue(), totalCount.get(e.getValue()) + 1);
            }
            else {
                totalCount.put(e.getValue(), 1);
            }
        }

        totalCount.remove(-1);
        System.out.println("MIN: " + min);
        System.out.println("MAX: " + max);
        for(Map.Entry<Integer, Point> e : points.entrySet()) {
            if(e.getValue().x == min.x || e.getValue().x == max.x || e.getValue().y == min.y || e.getValue().y == max.y) {
                System.out.println("Removing infinite: " + e.getKey() + " " + e.getValue());
                totalCount.remove(e.getKey());
            }
        }
        System.out.println(totalCount);
        System.out.println(totalCount.entrySet().stream().max(Map.Entry.comparingByValue()).get().getValue());


    }
}