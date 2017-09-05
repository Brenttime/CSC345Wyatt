/*
 * This class is made because we can't use ints or Integers (pass only by value and immutable respectively)
 * CSC 345 - Homework 0
 * Int.java
 * By: Brent Turner 
*/

public class Int 
{
	private int variable;
	
	public Int(int x)
	{
		variable = x;
	}
		
	public int getInt()
	{
		return variable;
	}
	
	public void setInt(int x)
	{
		variable = x;
	}
}
