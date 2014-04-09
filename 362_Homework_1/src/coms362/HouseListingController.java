package coms362;

import java.util.List;

public interface HouseListingController {

	public boolean createListing(String address, String status, String description);
	
	public boolean updateAgent(int hlId);
	
	public boolean updateDescription(int hlId, String description);
	
	public boolean hideListing(int hlId, boolean hidden);
	
	public List<HouseListing> searchAddress(String address);
	
	public List<HouseListing> searchPrice(int price);
	
	public List<HouseListing> searchStatus(String status);
	
	public boolean updateHouseAddress(String address, int hlId);
	
	public boolean updateHousePrice(int hlId, int price);
	
	public boolean updateHouseStatus(int hlId, String status);
}
