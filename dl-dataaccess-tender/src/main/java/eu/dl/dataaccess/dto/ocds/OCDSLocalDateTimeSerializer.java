package eu.dl.dataaccess.dto.ocds;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Datetime serializer.
 *
 * @author Tomas Mrazek
 */
public class OCDSLocalDateTimeSerializer extends StdSerializer<LocalDateTime> {
    private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");

    /**
     * Default constructor.
     */
    public OCDSLocalDateTimeSerializer() {
        super(LocalDateTime.class);
    }

    @Override
    public final void serialize(final LocalDateTime value, final JsonGenerator gen, final SerializerProvider provider)
        throws IOException {
        gen.writeString(value.format(formatter));
    }
}
