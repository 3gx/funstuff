package dataExtensibility.tests;

import static org.junit.Assert.*;
import org.junit.BeforeClass;
import org.junit.Test;
import dataExtensibility.*;

public class TestLit {

	private static Lit x;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		x = new Lit();
		x.setInfo(42);
	}
	
	@Test
	public void testPrettyPrint() {
		assertEquals("pretty print a literal", "42", x.prettyPrint());
	}

	@Test
	public void testEvaluate() {
		assertEquals("evaluate a literal", 42, x.evaluate());
	}
	
}
