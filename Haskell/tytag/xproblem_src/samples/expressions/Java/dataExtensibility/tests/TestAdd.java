package dataExtensibility.tests;

import static org.junit.Assert.*;
import org.junit.BeforeClass;
import org.junit.Test;
import dataExtensibility.*;

public class TestAdd {

	private static Add x;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		x = new Add();
		Lit y = new Lit();
		y.setInfo(1);
		x.setLeft(y);
		y = new Lit();
		y.setInfo(2);
		x.setRight(y);
	}
	
	@Test
	public void testPrettyPrint() {
		assertEquals("pretty print addition", "1 + 2", x.prettyPrint());
	}

	@Test
	public void testEvaluate() {
		assertEquals("evaluate addition", 3, x.evaluate());
	}

}
