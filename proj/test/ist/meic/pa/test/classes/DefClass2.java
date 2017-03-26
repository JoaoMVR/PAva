package ist.meic.pa.test.classes;

import ist.meic.pa.KeywordArgs;

public class DefClass2 extends DefClass {

    int b;

    public DefClass2() { }

    @KeywordArgs("b=20")
    public DefClass2(Object... args) { }

    public String toString() {
        return String.format("a:%s,b:%s", a, b);
    }

}

