import java.util.ArrayList;

public class LongestIncreasingSubsequence {
  
  public static void main(String[] args) {
    int[] a = new int[] {1,3,5,2,7};
    int[] b = new int[] {1,2,3,5,7};
    int[] c = lis(a, b);
    //=> 1,3,5,7
    for (int each : c) System.out.println(each);
  }

  public static int[] lis(int[] a, int[] b) {
    // construct new sequence where for each
    // position in the first sequence, we would
    // list its position in the second sequence
    ArrayList<Integer> ab = new ArrayList<Integer>();
    for (int i = 0; i < a.length; ++i)
      for (int j = 0; j < b.length; ++j)
        if (a[i] == b[j])
          ab.add(j + 1);

    // find longest non decreasing from the new sequence
    ArrayList<Integer> nondec = new ArrayList<Integer>();
    int highest_sofar = -1;
    for (int i = 0; i <= ab.size() - 1; i++) {
      int current = ab.get(i);      
      if ( highest_sofar < current ) {
        nondec.add(current);
        highest_sofar = current;
      }
    }

    // apply it as indices to the 2nd sequence
    int[] result = new int[nondec.size()];
    for (int i = 0; i <= nondec.size() - 1; i++) {
      int indices = nondec.get(i);
      result[i] = b[indices - 1];
    }
    return result;
  }
}