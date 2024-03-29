package ist.meic.pa;

import org.junit.Test;
import static org.junit.Assert.assertEquals;
import org.junit.runner.RunWith;

import ist.meic.pa.test.classes.*;

@RunWith(TestRunner.class)
public class FirstProjectTests {

    @Test
    public void testA() {
        assertEquals("width:100,height:50,margin:5",
                     new Widget().toString());
        assertEquals("width:80,height:50,margin:5" ,
                     new Widget("width", 80).toString());
        assertEquals("width:100,height:30,margin:5",
                     new Widget("height", 30).toString());
        assertEquals("width:100,height:50,margin:2",
                     new Widget("margin", 2).toString());
        assertEquals("width:8,height:13,margin:21" ,
                     new Widget("width", 8, "height", 13, "margin", 21).toString());
    }

    @Test
    public void testB() {
        assertEquals("width:200,height:50,margin:10,name:Extended",
                     new ExtendedWidget().toString());
        assertEquals("width:80,height:50,margin:10,name:Extended",
                     new ExtendedWidget("width", 80).toString());
        assertEquals("width:200,height:30,margin:10,name:Extended",
                     new ExtendedWidget("height", 30).toString());
        assertEquals("width:90,height:20,margin:10,name:Extended",
                     new ExtendedWidget("height", 20, "width", 90).toString());
        assertEquals("width:90,height:20,margin:10,name:Nice",
                     new ExtendedWidget("height", 20, "width", 90, "name", "Nice").toString());
    }

    @Test(expected=java.lang.RuntimeException.class)
    public void testC() {
        final String cause = "Unrecognized keyword: foo";

        try {
            new ExtendedWidget("foo", 1, "bar", 2);
        } catch (RuntimeException e) {
            assertEquals(cause, e.getMessage());
            throw e;
        }
    }

    @Test
    public void testD() {
        assertEquals("someNumber: 0, someChar:  , someDouble: 0.0",
                     new VoidWidget().toString());
    }

    @Test
    public void testE() {
        assertEquals("l: 10, b: 0, v :true",
                     new MixKeys().toString());
        assertEquals("l: 20000, b: 0, v :false",
                     new MixKeys("l", 20000, "v", false).toString());
    }

    @Test
    public void testF() {
        assertEquals("Interesting places: Lordran, Drangleic, Lothric",
                     new KeyPlaces().toString());
        assertEquals("visited: 0, places: Lordran, Drangleic, Lothric",
                     new KeyVisited().toString());
        assertEquals("visited: 2, places: Lordran, Yharnam, Lothric",
                     new KeyVisited("visited", 2, "second", "Yharnam").toString());
        assertEquals("visited: 0, places: Lordran, Drangleic, Pthumerian",
                     new KeyVisited("third", "Pthumerian").toString());
    }

    @Test
    public void testG() {
        assertEquals("Nothing...", new Unknown().toString());
    }

    @Test
    public void testH() {
        assertEquals("result:45.0,value:0.8509035245341184",
                     new FuncWidget().toString());
    }

    @Test(expected=java.lang.RuntimeException.class)
    public void nonExistentField() {
        final String cause = "Unrecognized keyword: x";

        try {
            Unknown u = new Unknown("x", 0);
        } catch (RuntimeException e) {
            assertEquals(cause, e.getMessage());
            throw e;
        }

    }

    @Test
    public void implementsInterface() {
        assertEquals("a:1,b:a,c:hello,width:100,height:25,margin:2",
                     new ImplementedWidget("margin", 2,
                                           "height", 25,
                                           "b", 'a').toString());
    }

    @Test
    public void severalAnnotations() {
        assertEquals("a:1,b:2", new SeveralAnnotations().toString());
        assertEquals("a:3,b:4", new SeveralAnnotations("b", 4, "a", 3).toString());
    }

    @Test 
    public void classHasDefaultConstructor() {
        assertEquals("a:10", new DefClass("a", 10).toString());
        assertEquals("a:10,b:20", new DefClass2("a", 10, "b", 20).toString());
    }

    @Test(expected=java.lang.RuntimeException.class)
    public void oddArgCount() {
        final String cause = "Odd argument count.";

        try {
            new Widget("width", 10, "height");
        } catch (RuntimeException e) {
            assertEquals(cause, e.getMessage());
            throw e;
        }
    }

    @Test
    public void defaultIsStringWithComma() {
        assertEquals("msg:Hello, world!", 
                     new DefaultWithComma().toString());
    }
}
