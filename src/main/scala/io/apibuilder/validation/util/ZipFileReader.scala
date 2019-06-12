package io.apibuilder.validation.util

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.nio.file.Files
import java.util.zip.ZipEntry
import java.util.zip.ZipInputStream

case class ZipFileReader(fileZip: File) {

  private[this] val destDir: File = Files.createTempDirectory("zipfilereader").toFile

  /**
    * Returns a list of the entries of the zip file
    */
  lazy val entries: Seq[ZipFileEntry] = {
    val all = scala.collection.mutable.ListBuffer[ZipFileEntry]()
    val buffer = new Array[Byte](1024)
    val zis = new ZipInputStream(new FileInputStream(fileZip))
    var zipEntry = zis.getNextEntry
    while (zipEntry != null) {
      val thisFile = newFile(zipEntry)
      all.append(
        ZipFileEntry(name = zipEntry.getName, file = thisFile)
      )
      val fos = new FileOutputStream(thisFile)
      var len = zis.read(buffer)
      while (len > 0) {
        fos.write(buffer, 0, len)
        len = zis.read(buffer)
      }
      fos.close()
      zipEntry = zis.getNextEntry
    }
    zis.closeEntry()
    zis.close()
    all
  }

  private[this] def newFile(zipEntry: ZipEntry): File = {
    val file = new File(destDir, zipEntry.getName)
    assert(
      file.getCanonicalPath.startsWith(destDir.getCanonicalPath + File.separator),
      s"Entry[${zipEntry.getName}] is outside of the target dir[${destDir.getCanonicalPath}]"
    )
    file
  }
}