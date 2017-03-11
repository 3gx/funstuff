package dataExtensibility;

/**
 * The expression form of "literals" (i.e., constants)
 */
public class Lit extends Expr {

	private int info;
	public int getInfo() { return info; }
	public void setInfo(int info) { this.info = info; }
	
	public String prettyPrint() {
		return Integer.toString(getInfo());
	}
	
	public int evaluate() {
		return getInfo();
	}
}
