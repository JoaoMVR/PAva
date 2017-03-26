package ist.meic.pa.test.classes;

import ist.meic.pa.KeywordArgs;

public class FuncWidget {
    double result;
    double value;

    @KeywordArgs("result=40+5,value=Math.sin(result)")
    public FuncWidget(Object... args) {}

    public String toString() {
        return String.format("result:%s,value:%s",
                             result, value);
    }
}
