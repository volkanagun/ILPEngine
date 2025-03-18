package ilp.gpu

import com.aparapi.Kernel
import com.aparapi.device.Device
import com.aparapi.internal.kernel.KernelManager
import com.aparapi.internal.opencl.OpenCLPlatform

object JoinManager {

  var totalMemorySize = Map[Long, Double]()
  var availableMemorySize = Map[Long, Double]()

  var devices = list()
  var switch = true

  def run(rangeX: Int, kernel: Kernel): Kernel = {
    val device = best()
    val range = com.aparapi.Range.create(device, rangeX, 1)
    kernel.execute(range)
  }

  def runWithoutDevice(rangeX: Int, kernel: Kernel): Kernel = {
    val range = com.aparapi.Range.create(rangeX, 1)
    kernel.execute(range)
  }

  def run(rangeX: Int, rangeY: Int, kernel: Kernel): Kernel = {
    val device = best()
    val range = com.aparapi.Range.create2D(device, rangeX, rangeY, 1, 1)
    kernel.execute(range)

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

  def best(): Device = {
    val device = if (switch) devices.head
    else devices.tail.head
    switch = !switch
    device
  }

  def main(_args: Array[String]): Unit = {
    System.out.println("com.aparapi.examples.info.Main")

    val platforms = (new OpenCLPlatform()).getOpenCLPlatforms().toArray[OpenCLPlatform](Array[OpenCLPlatform]())
    System.out.println("Machine contains " + platforms.size + " OpenCL platforms")
    var platformc = 0

    for (platform <- platforms) {
      System.out.println("Platform " + platformc + "{")
      System.out.println("   Name    : \"" + platform.getName + "\"")
      System.out.println("   Vendor  : \"" + platform.getVendor + "\"")
      System.out.println("   Version : \"" + platform.getVersion + "\"")
      val devices = platform.getOpenCLDevices().toArray[Device](Array[Device]())
      System.out.println("   Platform contains " + devices.size + " OpenCL devices")
      var devicec = 0

      for (device <- devices) {
        System.out.println("   Device " + devicec + "{")
        System.out.println("       Type                  : " + device.getType)
        System.out.println("       MaxWorkGroupSizes     : " + device.getMaxWorkGroupSize)
        System.out.println("       MaxWorkItemDimensions : " + device.getMaxWorkItemDimensions)
        System.out.println("       Description : " + device.getShortDescription)
        System.out.println("   }")
        devicec += 1
      }

      System.out.println("}")
      platformc += 1
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
