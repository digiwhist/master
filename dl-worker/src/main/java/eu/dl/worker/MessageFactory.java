package eu.dl.worker;

/**
 * Returns new message to be send via workers.
 * 
 * @author Kuba Krafka
 */
public final class MessageFactory {
    
    /**
     * Private constructor, this class is a factory and shouldnt be initialized at all.
     */
    private MessageFactory() {
        // don't instantiate this class
    }
    
    /**
     * Returns empty message.
     * 
     * @return empty message
     */
    public static Message getMessage() {
        return new SimpleMessage();
    }
    
    /**
     * Return initialized message with data from json.
     * 
     * @param json message data
     * @return message with data
     */
    public static Message getMessage(final String json) {
        Message message = new SimpleMessage();
        message.init(json);
        return message;
    }
}
