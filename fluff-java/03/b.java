import java.awt.Rectangle;
import java.util.*;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

class Day03B {

    public static void main(String[] args) {
        Pattern pattern = Pattern.compile("^\\#(\\d+)\\ \\@\\ (\\d+)\\,(\\d+)\\:\\ (\\d+)x(\\d+)$");

        HashMap<Integer, Claim> claims = new HashMap<>();

        Scanner in = new Scanner(System.in);
        while(in.hasNext()) {
            String str = in.nextLine();
            Matcher matcher = pattern.matcher(str);
            while(matcher.find()) {
                int id = Integer.valueOf(matcher.group(1));
                int x = Integer.valueOf(matcher.group(2));
                int y = Integer.valueOf(matcher.group(3));
                int width = Integer.valueOf(matcher.group(4));
                int height = Integer.valueOf(matcher.group(5));

                Claim claim = new Claim(id, x, y, width, height);

                for(Map.Entry<Integer, Claim> entry : claims.entrySet()) {
                    Claim claim2 = entry.getValue();
                    if(claim.intersects(claim2) > 0) {
                        claim.increaseIntersectCount();
                        claim2.increaseIntersectCount();
                        claims.put(entry.getKey(), claim2);
                    }
                }

                claims.put(claim.getId(), claim);
            }
        }
        in.close();



        List<Integer> nonIntersectingClaims = claims
                .entrySet()
                .stream()
                .filter(t -> t.getValue().getIntersectCount() == 0)
                .map(t -> t.getKey())
                .collect(Collectors.toList());

        System.out.println(nonIntersectingClaims.toString());
    }
}

class Claim {
    private int id = 0;
    private Rectangle rect;
    private int intersectCount = 0;

    public Claim(int id, int x, int y, int width, int height) {
        this.id = id;
        this.rect = new Rectangle(x, y, width, height);
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public Rectangle getRect() {
        return rect;
    }

    public void setRect(Rectangle rect) {
        this.rect = rect;
    }

    public int increaseIntersectCount() {
        this.intersectCount++;
        return this.intersectCount;
    }

    public int getIntersectCount() {
        return intersectCount;
    }

    public int intersects(Claim claim2) {
        Rectangle intersection = this.getRect().intersection(claim2.getRect());
        if(intersection.height > 0 && intersection.width > 0) {
            return intersection.height * intersection.width;
        }
        else {
            return 0;
        }
    }
}

