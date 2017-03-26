package ist.meic.pa.test.classes;

import ist.meic.pa.KeywordArgs;

public class SeveralAnnotations {
    int a, b;

    @KeywordArgs("a=1,b=2")
    public SeveralAnnotations(Object... args) {}

    @KeywordArgs("")
    public SeveralAnnotations() {}

    @KeywordArgs("a=2,b=3")
    public SeveralAnnotations(int a, int b) {}

    public String toString() {
        return String.format("a:%s,b:%s", a, b);
    }

}
