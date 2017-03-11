package operationExtensibility.tests;

import static org.junit.Assert.assertEquals;
import org.junit.BeforeClass;
import org.junit.Test;
import operationExtensibility.*;

public class TestEvaluator {

	private static Visitor<Integer> v;
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		v = new Evaluator();
	}
	
	@Test
	public void testLit() {
		Lit x = new Lit();
		x.setInfo(42);
		assertEquals("evaluate a literal", 42, x.accept(v));
	}
	
	@Test
	public void testAdd() {
		Add x = new Add();
		Lit y = new Lit();
		y.setInfo(1);
		x.setLeft(y);
		y = new Lit();
		y.setInfo(2);
		x.setRight(y);
		assertEquals("evaluate addition", 3, x.accept(v));
	}	
}
