package operationExtensibility;

/**
 * Operation for pretty printing
 */
public class PrettyPrinter extends Visitor<String> {
	public String visit(Lit x) {
			return Integer.toString(x.getInfo());
	}
	public String visit(Add x) {
			return x.getLeft().accept(this)
			     + " + "
			     + x.getRight().accept(this);
	}
	public String visit(Neg x) {
		return "- (" + x.getExpr().accept(this) + ")";
	}
}
