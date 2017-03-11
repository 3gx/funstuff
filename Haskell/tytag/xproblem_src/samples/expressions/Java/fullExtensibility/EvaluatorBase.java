package fullExtensibility;

public class EvaluatorBase {

	public int evaluate(Expr e) {
		if (e instanceof Lit) {
			Lit l = (Lit)e;
			return l.getInfo();
		}
		if (e instanceof Add) {
			Add a = (Add)e;
			return evaluate(a.getLeft()) + evaluate(a.getRight());
		}		
		throw new FallThrouhException();
	}
}
