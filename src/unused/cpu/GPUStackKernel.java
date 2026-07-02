package ilp.cpu;


import com.aparapi.Kernel;

public abstract class GPUStackKernel extends Kernel {

    protected int stackSize = 1000;
    protected int[] stackPointer;
    protected int[][] stack;
    protected int[][][] stackPositions;

    public GPUStackKernel(int indexSize){
        this.stackPointer = new int[indexSize];
        this.stack = new int[indexSize][stackSize];
        this.stackPositions = new int[indexSize][stackSize][100];
    }

    public void push(int index, int value){
        stack[index][stackPointer[index]] = value;
    }

    public void pushArray(int index, int[] value){
        stackPositions[index][stackPointer[index]] = value;
    }

    public int pop(int index){
        int value = stack[index][stackPointer[index]];
        return value;
    }
    public int[] popArray(int index){
        int[] value = stackPositions[index][stackPointer[index]];
        return value;
    }

    public int increment(int index){
        return ++stackPointer[index];
    }
    public int decrement(int index){
        return --stackPointer[index];
    }
    public boolean notEmpty(int index){
        return stackPointer[index]>0;
    }
    public boolean hasOne(int index){
        return stackPointer[index]==1;
    }
}