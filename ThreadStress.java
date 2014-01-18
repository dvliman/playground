/* warning: this can kill your system */
public class ThreadStress {
  private static Object s = new Object(); 
  private static int count = 0;; 
  
  public static void main(String[] args) {
    for(;;) {
      new Thread(new Runnable() {
        public void run() {
          synchronized(s) {
            count++;
            System.err.println("New Thread #" + count);
          }
          for(;;) {
            try {
              Thread.sleep(1000);
            } catch (Exception e) {
              System.err.println(e);
            }
          }
        }
      }).start();
    }
  }
}
