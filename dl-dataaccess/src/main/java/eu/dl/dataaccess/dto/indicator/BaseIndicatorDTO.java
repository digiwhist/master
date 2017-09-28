package eu.dl.dataaccess.dto.indicator;

import eu.dl.dataaccess.dto.StorableDTO;

/**
 * Basic class for indicators.
 * 
 */
public abstract class BaseIndicatorDTO extends StorableDTO implements Indicator {
    private String type;

    @Override
    public final String getType() {
        return type;
    }

    @Override
    public final void setType(final String newType) {
        this.type = newType;
    }
}
