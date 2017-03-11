package operationExtensibility;

/**
 * A concrete visitor describe a concrete operation on expressions.
 * There is one visit method per type in the class hierarchy.
 */
public abstract class Visitor<R> {
	
	public abstract R visit(Lit x);
	public abstract R visit(Add x);
	public abstract R visit(Neg x);
}
