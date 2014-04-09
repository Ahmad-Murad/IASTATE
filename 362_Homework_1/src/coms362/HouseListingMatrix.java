package coms362;

import java.util.List;

public interface HouseListingMatrix {
	
	public boolean createListing(String address, String status, String description);
	
	public boolean updateAgent(HouseListing hl);
	
	public boolean updateDescription(HouseListing hl);
	
	public boolean hideListing(HouseListing hl, boolean hidden);
	
	public boolean updateHouseAddress(int hlId, String address);
	
	public List<HouseListing> searchAddress(String status);
	
	public List<HouseListing> searchPrice(int price);
	
	public List<HouseListing> searchStatus(String status);
	
	public boolean updateHousePrice(int hlId, int newPrice);
	
	public boolean getHouseListing(int hlId);
}
