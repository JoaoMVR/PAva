package ist.meic.pa;

public class ImplementedWidget extends Widget implements IA {
    int a;
    char b;
    String c;

    @KeywordArgs("a=1,b=2,c=\"hello\"")
    public ImplementedWidget(Object... args) { }

    public String toString() {
        return String.format("a:%s,b:%s,c:%s,width:%s,height:%s,margin:%s",
                             a, b, c, width, height, margin);
    }

}
