
public class ReverseSinglyLinkedList {
  
  public static void main(String[] args) {
    //=> Singly linked list of 1,2,3,4,5
    Node node5 = new Node(5);
    Node node4 = new Node(4, node5);
    Node node3 = new Node(3, node4);
    Node node2 = new Node(2, node3);
    Node node1 = new Node(1, node2);

    // the algorithm to reverse a singly linked list
    // the idea is to save the pointer to the next 
    // node before overwriting it.
    Node prev = null;
    Node current = node1;
    while (current != null) {
      Node temp = current.next; 
      current.next = prev; 
      prev = current;
      current = temp;
    }
    
    //=> prints 5,4,3,2,1
    for (Node result = prev; result != null; result = result.next)
      System.out.println(result);
  }
  
  /** a singly node data structure */
  public static class Node {
    public Integer data; 
    public Node next; 
    
    public Node(Integer data) {
      this(data, null);
    }
    
    public Node(Integer data, Node next) {
      this.data = data; 
      this.next = next; 
    }
    
    public String toString() {
      return data.toString();
    }
  }
}