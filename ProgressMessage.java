

public class ProgressMessage {
  public static void main(String[] args) {
    for (int i = 0; i < 500; i++) {
      String msg = "Progress: " + i + "%";
      System.out.print(msg);
      
      for (int j = 0; j < msg.length(); i++)
    	System.out.print("\b");
	}
  }
}
