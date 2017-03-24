package ist.meic.pa;

import org.junit.Test;
import static org.junit.Assert.assertEquals;
import org.junit.runner.RunWith;

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
    public void TestB() {
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
    public void TestC() {
        String error =
              "Exception in thread \"main\" java.lang.RuntimeException: Unrecognized keyword: foo" +         "\n"
            + "  at ExtendedWidget.<init>(ExtendedWidget.java)" +                                            "\n"
            + "  at TestC.main(TestC.java:3)" +                                                              "\n"
            + "  at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)" +                           "\n"
            + "  at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)" +         "\n"
            + "  at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)" + "\n"
            + "  at java.lang.reflect.Method.invoke(Method.java:497)" +                                      "\n"
            + "  at javassist.Loader.run(Loader.java:288)" +                                                 "\n"
            + "  at ist.meic.pa.KeyConstructors.main(Unknown Source)" +                                      "\n";

        assertEquals(error, new ExtendedWidget("foo", 1, "bar", 2));
    }

    @Test
    public void TestD() {
        assertEquals("someNumber: 0, someChar:  , someDouble: 0.0",
                     new VoidWidget().toString());
    }

    @Test
    public void TestE() {
        assertEquals("l: 10, b: 0, v :true",
                     new MixKeys().toString());
        assertEquals("l: 20000, b: 0, v :false",
                     new MixKeys("l", 20000, "v", false).toString());
    }

    @Test
    public void TestF() {
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
    public void TestG() {
        assertEquals("Nothing...", new Unknown().toString());
    }

    @Test
    public void TestH() {
        assertEquals("result:45.0,value:0.8509035245341184",
                     new FuncWidget().toString());
    }

}
