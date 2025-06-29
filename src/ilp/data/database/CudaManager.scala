package ilp.data.database

import com.aparapi.Kernel
import com.aparapi.device.Device
import com.aparapi.internal.kernel.KernelManager
import com.aparapi.internal.opencl.OpenCLPlatform

import java.util

object CudaManager {

  var totalMemorySize: Map[Long, Double] = Map[Long, Double]()
  var availableMemorySize: Map[Long, Double] = Map[Long, Double]()
  private var devices: Array[Device] = list()
  private var switch = true

  def setCPU():this.type =
    val deviceList = new util.LinkedHashSet[Device]()
    deviceList.add(devices.tail.head)
    KernelManager.instance().setDefaultPreferredDevices(deviceList)
    this

  def setGPU():this.type =
    val deviceList = new util.LinkedHashSet[Device]()
    deviceList.add(devices.head)
    KernelManager.instance().setDefaultPreferredDevices(deviceList)
    this

  def run(rangeX: Int, kernel: Kernel): Kernel = {
    val device = best()
    val range = com.aparapi.Range.create(device, rangeX, 1)
    kernel.execute(range)
  }

  def runWithoutDevice(rangeX: Int, kernel: Kernel): Kernel = {
    val range = com.aparapi.Range.create(rangeX, 1)
    kernel.execute(range)
  }
  def runWithoutDevice(rangeX: Int,rangeY: Int, kernel: Kernel): Kernel = {
    val range = com.aparapi.Range.create2D(rangeX,rangeY, 1, 1)
    kernel.execute(range)
  }

  def run(rangeX: Int, rangeY: Int, kernel: Kernel): Kernel = {
    val device = best()
    val range = com.aparapi.Range.create2D(device, rangeX, rangeY, 1, 1)
    kernel.execute(range)
  }

  def runCPU(rangeX: Int, rangeY: Int, kernel: Kernel): Kernel = {
    val device = devices.last
    val range = com.aparapi.Range.create2D(device, rangeX, rangeY, 1, 1)
    kernel.execute(range)
    kernel
  }

  def runAny(rowSize: Int, valueSize: Int, tableSize: Int, kernel: Kernel): Kernel = {
    val range = com.aparapi.Range.create3D(rowSize, valueSize, tableSize, 1, 1, 1)
    kernel.execute(range)
    kernel
  }

  def runReduced(rowSize: Int, tableSize: Int, kernel: Kernel): Kernel = {
    val range = com.aparapi.Range.create2D(rowSize, tableSize, 1, 1)
    kernel.execute(range)
    kernel
  }

  def runLocal(rowSize: Int, valueSize: Int, tableSize: Int, kernel: Kernel): Kernel = {
    val roundRowSize = rowSize * tableSize
    val range = com.aparapi.Range.create2D(roundRowSize, valueSize, tableSize, 1)
    kernel.execute(range)
    kernel
  }

  def runVeryLocal(rowSize: Int, valueSize: Int, tableSize: Int, localSize:Int, kernel: Kernel): Kernel = {
    val roundRowSize = rowSize
    val roundValueSize = valueSize

    val range = com.aparapi.Range.create2D(roundRowSize, roundValueSize, 1, 1)
    kernel.execute(range)
    kernel
  }
  def runAny(rangeX: Int, rangeY: Int, kernel: Kernel): Kernel = {
    val range = com.aparapi.Range.create2D(rangeX, rangeY, 1, 1)
    kernel.execute(range)
    kernel
  }
  def runSecond(rangeX: Int, rangeY: Int, kernel: Kernel): Kernel = {
    val device = devices.tail.head

    val range = com.aparapi.Range.create2D(device, rangeX, rangeY, 1, 1)
    kernel.execute(range)
  }

  def run(rangeX: Int, rangeY: Int, rangeZ: Int, kernel: Kernel): Kernel = {
    val device = best()
    val range = com.aparapi.Range.create3D(device, rangeX, rangeY, rangeZ, 1, 1, 1)
    kernel.execute(range)
  }

  def run(rangeX: Int, rangeY: Int, rangeZ: Int, rangeW:Int, kernel: Kernel): Kernel = {
    val device = best()
    val range = com.aparapi.Range.create3D(device, rangeX, rangeY, rangeZ, rangeW, 1, 1)
    kernel.execute(range)
  }


  def list(): Array[Device] = {
    val preferences = KernelManager.instance.getDefaultPreferences
    preferences.getPreferredDevices(null).toArray[Device](Array[Device]())
  }

  private def best(): Device = {
    val device = if (switch) devices.head
    else devices.tail.head
    switch = !switch
    device
  }

  def main(_args: Array[String]): Unit = {

    val destination = Array.fill[Int](200)(0)
    val source = Range(0, 10).toArray
    Array.copy(source, 0, destination, 0, source.length)

    System.out.println("com.aparapi.examples.info.Main")

    val platforms = new OpenCLPlatform().getOpenCLPlatforms().toArray[OpenCLPlatform](Array[OpenCLPlatform]())
    System.out.println("Machine contains " + platforms.size + " OpenCL platforms")
    var platformCount = 0

    for (platform <- platforms) {
      System.out.println("Platform " + platformCount + "{")
      System.out.println("   Name    : \"" + platform.getName + "\"")
      System.out.println("   Vendor  : \"" + platform.getVendor + "\"")
      System.out.println("   Version : \"" + platform.getVersion + "\"")
      val devices = platform.getOpenCLDevices().toArray[Device](Array[Device]())
      System.out.println("   Platform contains " + devices.size + " OpenCL devices")
      var deviceCount = 0

      for (device <- devices) {
        System.out.println("   Device " + deviceCount + "{")
        System.out.println("       Type                  : " + device.getType)
        System.out.println("       MaxWorkGroupSizes     : " + device.getMaxWorkGroupSize)
        System.out.println("       MaxWorkItemDimensions : " + device.getMaxWorkItemDimensions)
        System.out.println("       Description : " + device.getShortDescription)
        System.out.println("   }")
        deviceCount += 1
      }

      System.out.println("}")
      platformCount += 1
    }
    val preferences = KernelManager.instance.getDefaultPreferences()
    System.out.println("\nDevices in preferred order:\n")
    val deviceArray = preferences.getPreferredDevices(null).toArray[Device](Array[Device]())

    for (device <- deviceArray) {

      System.out.println(device)
      System.out.println()
    }
  }
}
