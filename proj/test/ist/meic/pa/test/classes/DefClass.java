package ist.meic.pa.test.classes;

import ist.meic.pa.KeywordArgs;

public class DefClass {

    int a;

    public DefClass() { }

    @KeywordArgs("a=10")
    public DefClass(Object... args) { }

    public String toString() {
        return String.format("a:%s", a);
    }

}
