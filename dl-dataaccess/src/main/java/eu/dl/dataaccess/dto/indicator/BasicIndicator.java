package eu.dl.dataaccess.dto.indicator;


import eu.dl.dataaccess.annotation.Transformable;

/**
 * Basic implementation of entity specific indicator.
 *
 */
@Transformable
public class BasicIndicator extends BaseIndicatorDTO implements Indicator {
	private String type;
	
	private IndicatorStatus status;
	
	private Double value;
	
    @Override
    public final String getType() {
        return type;
    }

    @Override
    public final void setType(final String newType) {
        this.type = newType;
    }
    
    @Override
    public final IndicatorStatus getStatus() {
        return status;
    }

    @Override
    public final void setStatus(final IndicatorStatus newStatus) {
        this.status = newStatus;
    }
    
    @Override
    public final Double getValue() {
        return value;
    }

    @Override
    public final void setValue(final Double newValue) {
        this.value = newValue;
    }
}
