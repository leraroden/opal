public class MyFirstExample {
    public static void main(String[] args) {
        int a = 1;
        int b = 2;
        int c = a + b;       // <-- `a` und `b` liegen kurzzeitig auf dem Stack
        int d = a + 5;       // <-- `a` wird ein zweites Mal gebraucht!
        System.out.println(c + d);
    }


//    int a = 1;
//    int b = 2;
//    int c = a + b;       // <-- `a` und `b` liegen kurzzeitig auf dem Stack
//        if(c == 3) {
//        int d = a + 5;       // <-- `a` wird ein zweites Mal gebraucht!
//        System.out.println(c + d);
//    }
//    int e = b + c;
}