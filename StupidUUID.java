public class StupidUUID {
  public static void main(String[] args) {
    Long id = System.currentTimeMillis() + Math.round(Math.random() * 1000000000000L); 
    System.out.println(id.toString());
  }
}