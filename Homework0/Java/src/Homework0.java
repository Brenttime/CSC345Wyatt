/*
 * CSC 345 - Homework 0
 * By: Brent Turner
 * Homework0.java
 */
public class Homework0 
{
	public static void main(String[] args) 
	{
		Int x = new Int(3);
		int y  = square(x) + twice(x) + once(x); //y == 45, not 18
		System.out.println(y);					//prints 45, not 18
	}
	
	private static int square(Int x) //x = x*x
	{
		x.setInt(x.getInt() * x.getInt());
		return x.getInt();
	}
	private static int twice(Int x) // x = x * 2
	{
		x.setInt(x.getInt() * 2);
		return x.getInt();
	}
	private static int once(Int x) // x = x
	{
		return x.getInt();
	}
}
