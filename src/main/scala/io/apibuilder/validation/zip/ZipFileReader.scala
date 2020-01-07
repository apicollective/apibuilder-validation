package io.apibuilder.validation.zip

import java.io.{BufferedInputStream, File, FileInputStream, FileOutputStream, InputStream}
import java.nio.file.Files
import java.util.zip.ZipEntry
import java.util.zip.ZipInputStream

import io.apibuilder.validation.util.UrlDownloader

object ZipFileReader {

  /**
    * Returns true if url path ends with .zip
    */
  def isZipFile(name: String): Boolean = endsWithSuffix(name, "zip")

  /**
    * Returns true if url path ends with .json
    */
  def isJsonFile(name: String): Boolean = endsWithSuffix(name, "json")

  private[this] def endsWithSuffix(name: String, suffix: String) = {
    name.trim.split("\\?").head.trim.toLowerCase().endsWith(s".$suffix")
  }

  def fromUrl(url: String): Either[Seq[String], ZipFileReader] = {
    UrlDownloader.withInputStream(url) { is =>
      Right(ZipFileReader(is))
    }
  }

  def fromFile(file: File): ZipFileReader = {
    ZipFileReader(new BufferedInputStream(new FileInputStream(file)))
  }
}

case class ZipFileReader(inputStream: InputStream) {

  private[this] val destDir: File = Files.createTempDirectory("zipfilereader").toFile

  /**
    * Returns a list of the entries of the zip file (all files ending with .json)
    */
  val entries: Seq[ZipFileEntry] = {
    val all = scala.collection.mutable.ListBuffer[ZipFileEntry]()
    val buffer = new Array[Byte](1024)
    val zis = new ZipInputStream(inputStream)
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
    all.toSeq
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