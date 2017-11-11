/*
 * This class is made because we can't use ints or Integers (pass only by value and immutable respectively)
 * CSC 345 - Homework 0
 * Int.java
 * By: Brent Turner
 * 09/04/17 
*/
public class Int 
{
	private int variable; //private x
	
	/*
	 * constructor
	 * @param x
	 */
	public Int(int x)
	{
		variable = x;
	}
		
	/*
	 * getter
	 * @return variable
	 */
	public int getInt()
	{
		return variable;
	}
	
	/*
	 * setter
	 * @param x
	 */
	public void setInt(int x)
	{
		variable = x;
	}
}
