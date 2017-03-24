import ist.meic.pa.KeywordArgs;


public class SuperExtendedWidget extends Widget {
	String superExtended;
	
	@KeywordArgs("width=200,margin=10,height=400, superExtended=\"YesIamSuperExtended")
	public SuperExtendedWidget(Object... args){}
	
	public String toString() {
		return String.format("width:%s,height:%s,margin:%s,superExtended=%s",
				width, height, margin, superExtended);
	}
}
