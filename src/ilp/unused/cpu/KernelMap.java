package ilp.cpu;

import java.io.Serializable;
import java.util.HashMap;

public class KernelMap implements Serializable {
    public HashMap<Integer, GPUBatchFilter> map = new HashMap<>();


    public KernelMap register(Integer id, GPUBatchFilter filter){
        map.put(id, filter);
        return this;
    }
    public GPUBatchFilter get(Integer id){
        return map.get(id);
    }

}
