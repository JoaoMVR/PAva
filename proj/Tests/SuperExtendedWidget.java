import ist.meic.pa.KeywordArgs;


public class SuperExtendedWidget extends ExtendedWidget {
	String superExtended;
	
	@KeywordArgs("margin=10,height=400, superExtended=\"YesIamSuperExtended\" ")
	public SuperExtendedWidget(Object... args){}
	
	public String toString() {
		return String.format("width:%s,height:%s,margin:%s,superExtended=%s",
				width, height, margin, superExtended);
	}
}
