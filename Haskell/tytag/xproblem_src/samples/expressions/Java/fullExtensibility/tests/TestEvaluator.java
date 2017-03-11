package fullExtensibility.tests;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import fullExtensibility.*;

public class TestEvaluator {
			
	@Test
	public void testEvaluatorExtension() {
		EvaluatorExtension e = new EvaluatorExtension();
		Add a = new Add();
		Lit l1 = new Lit();
		Lit l2 = new Lit();
		l1.setInfo(1);
		l2.setInfo(2);
		a.setLeft(l1);
		a.setRight(l2);
		Neg n = new Neg();
		n.setExpr(a);
		assertEquals("evaluate evaluation", -3, e.evaluate(n));		
	}
}
