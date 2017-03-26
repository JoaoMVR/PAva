package ist.meic.pa.test.classes;

import ist.meic.pa.KeywordArgs;

public class DefaultWithComma {
    String msg;

    @KeywordArgs("msg=\"Hello, world!\"")
    public DefaultWithComma(Object... args) { }

    public String toString() {
        return String.format("msg:%s", msg);
    }

}
