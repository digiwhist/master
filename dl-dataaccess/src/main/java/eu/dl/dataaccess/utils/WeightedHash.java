package eu.dl.dataaccess.utils;

/**
 * This class represents hash with height.
 * 
 * @author skajrajdr
 *
 */
public class WeightedHash {

	private String hash;
	
	private Double weight;

	/**
	 * Getter.
	 * @return hash value
	 */
	public final String getHash() {
		return hash;
	}

	/**
	 * Setter.
	 * @param hash hash value
	 * @return this for fluent
	 */
	public final WeightedHash setHash(final String hash) {
		this.hash = hash;
		return this;
	}

	/**
	 * Getter.
	 * @return hash weight
	 */
	public final Double getWeight() {
		return weight;
	}

	/**
	 * Setter.
	 * @param weight hash weight
	 * @return this for fluent
	 */
	public final WeightedHash setWeight(final Double weight) {
		this.weight = weight;
		return this;
	}
}
