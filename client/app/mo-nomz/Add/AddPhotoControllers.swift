//
//  AddPhotoControllers.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/31/22.
//

import MobileCoreServices
import UIKit
import Vision
import PhotosUI
import QCropper

class AddPhotoController: AddDetailController {
    @IBOutlet weak var helper: UIButton!

    var scrape: Scrape = Scrape<ScrapeImageInfo>(ingredients: [], steps: [])
    var current: ScrapeImageInfo? = nil
    var pickVc: PickPhotoController? = nil
    
    override func addType() -> AddType {
        return .photo
    }
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        performSegue(withIdentifier: "pushManualRecipe", sender: nil)
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? PickPhotoController, segue.identifier == "embedPhotos" {
            pickVc = vc
            vc.addVc = self
        }
        if let vc = segue.destination as? AddManualController, segue.identifier == "pushManualRecipe" {
            let ingredients = scrape.ingredients.map({ $0.value }).joined(separator: "\n")
            let steps = scrape.steps.map({ $0.value }).joined(separator: "\n").nonEmpty()
            vc.change = .photo(ingredients, steps)
        }
        if let vc = segue.destination as? ReviewPhotoController, segue.identifier == "reviewScrape" {
            vc.addVc = self
            vc.scrapeInfo = current
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        helper.menu = switcherMenu()
    }
}

class PickPhotoController: UICollectionViewController, UICollectionViewDelegateFlowLayout, UIImagePickerControllerDelegate, UINavigationControllerDelegate, CropperViewControllerDelegate {
    var scrapeType: ScrapeType? = nil
    var addVc: AddPhotoController? = nil
    
    let INGREDIENTS = 0
    let STEPS = 1
    
    @objc func didTapAddPhoto(_ sender: Any?) {
        guard let b = sender as? UIButton else { return }
        switch b.tag {
        case INGREDIENTS: scrapeType = .ingredient
        case STEPS: scrapeType = .step
        default: return
        }
        let picker = UIImagePickerController()
        picker.sourceType = .photoLibrary
        picker.mediaTypes = [kUTTypeImage as String]
        picker.delegate = self
        if UIDevice.current.userInterfaceIdiom == .pad {
            picker.modalPresentationStyle = .popover
            picker.popoverPresentationController?.sourceView = b
        }
        present(picker, animated: true)
    }
    
    override func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 2
    }
    
    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        switch section {
        case INGREDIENTS: return (addVc?.scrape.ingredients.count ?? 0) + 1
        case STEPS: return (addVc?.scrape.steps.count ?? 0) + 1
        default: return 0
        }
    }
    
    private func imageCell(_ collectionView: UICollectionView, indexPath: IndexPath, scrapeInfos: [ScrapeImageInfo]) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "imageItem", for: indexPath) as! OneCellImage
        cell.layer.cornerRadius = 10
        cell.image.image = scrapeInfos[indexPath.row].image
        return cell
    }
    
    private func addCell(_ collectionView: UICollectionView, indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "addImage", for: indexPath) as! OneCellButton
        cell.layer.cornerRadius = 10
        cell.button.tag = indexPath.section
        cell.button.addTarget(self, action: #selector(didTapAddPhoto), for: .touchUpInside)
        return cell
    }
    
    private func isIngredient(_ indexPath: IndexPath) -> Bool {
        return indexPath.section == INGREDIENTS && indexPath.row < addVc!.scrape.ingredients.count
    }
    
    private func isStep(_ indexPath: IndexPath) -> Bool {
        return indexPath.section == STEPS && indexPath.row < addVc!.scrape.steps.count
    }
    
    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        if isIngredient(indexPath) {
            return imageCell(collectionView, indexPath: indexPath, scrapeInfos: addVc!.scrape.ingredients)
        } else if isStep(indexPath) {
            return imageCell(collectionView, indexPath: indexPath, scrapeInfos: addVc!.scrape.steps)
        }
        return addCell(collectionView, indexPath: indexPath)
    }
    
    override func collectionView(_ collectionView: UICollectionView, contextMenuConfigurationForItemAt: IndexPath, point: CGPoint) -> UIContextMenuConfiguration? {
        let handler: (() -> Void)
        let image: UIImage
        switch contextMenuConfigurationForItemAt.section {
        case INGREDIENTS:
            image = self.addVc!.scrape.ingredients[contextMenuConfigurationForItemAt.row].image
            handler = { self.addVc?.scrape.ingredients.remove(at: contextMenuConfigurationForItemAt.row) }
            break
        case STEPS:
            image = self.addVc!.scrape.steps[contextMenuConfigurationForItemAt.row].image
            handler = { self.addVc?.scrape.steps.remove(at: contextMenuConfigurationForItemAt.row) }
            break
        default: return nil
        }
        let preview: UIContextMenuContentPreviewProvider = {
            return PreviewPhotoController(image: image)
        }
        let actions: UIContextMenuActionProvider = { _ in
            return UIMenu(children: [
                UIAction(title: "Remove image", image: UIImage(systemName: "xmark"), attributes: .destructive, handler: { _ in
                    handler()
                    DispatchQueue.main.async {
                        self.collectionView.reloadData()
                    }
                })
            ])
        }
        return UIContextMenuConfiguration(identifier: nil, previewProvider: preview, actionProvider: actions)
    }
    
    override func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        if isIngredient(indexPath) {
            self.addVc?.current = addVc?.scrape.ingredients[indexPath.row]
            self.addVc?.performSegue(withIdentifier: "reviewScrape", sender: nil)
        } else if isStep(indexPath) {
            self.addVc?.current = addVc?.scrape.steps[indexPath.row]
            self.addVc?.performSegue(withIdentifier: "reviewScrape", sender: nil)
        } else {
            let cell = collectionView.cellForItem(at: indexPath) as! OneCellButton
            didTapAddPhoto(cell.button)
        }
    }
    
    func collectionView(_ collectionView: UICollectionView, layout: UICollectionViewLayout, sizeForItemAt: IndexPath) -> CGSize {
        let size = (view.frame.size.width / 3) - 15
        return CGSize(width: size, height: size)
    }
    
    func collectionView(_ collectionView: UICollectionView, layout: UICollectionViewLayout, insetForSectionAt: Int) -> UIEdgeInsets {
        return UIEdgeInsets(top: 10, left: 10, bottom: 10, right: 10)
    }
    
    override func collectionView(_ collectionView: UICollectionView, viewForSupplementaryElementOfKind kind: String, at indexPath: IndexPath) -> UICollectionReusableView {
        let header = collectionView.dequeueReusableSupplementaryView(ofKind: kind, withReuseIdentifier: "collectionHeader", for: indexPath) as! CollectionHeader
        switch indexPath.section {
        case INGREDIENTS:
            header.label.text = "Ingredients"
            break
        case STEPS:
            header.label.text = "Steps"
            break
        default: break
        }
        return header
    }
    
    func imagePickerController(_ picker: UIImagePickerController, didFinishPickingMediaWithInfo info: [UIImagePickerController.InfoKey : Any]) {
        guard let image = info[.originalImage] as? UIImage else { return }
        let cropper = CropperViewController(originalImage: image)
        cropper.delegate = self
        picker.dismiss(animated: true) {
            self.present(cropper, animated: true, completion: nil)
        }
    }

    func cropperDidConfirm(_ cropper: CropperViewController, state: CropperState?) {
        cropper.dismiss(animated: true, completion: nil)
        let progress = startLoading()
        let cropped = cropper.originalImage.cropped(withCropperState: state!)!
        let textHandler = { (req: VNRequest, error: Error?) in
            self.stopLoading(progress)
            guard let observations = req.results as? [VNRecognizedTextObservation] else { return }
            let recognized = observations.compactMap({ $0.topCandidates(1).first?.string }).joined(separator: "\n")
            self.addVc?.current = ScrapeImageInfo(image: cropped, value: recognized, type: self.scrapeType ?? .ingredient)
            self.addVc?.performSegue(withIdentifier: "reviewScrape", sender: nil)
        }
        let handler = VNImageRequestHandler(cgImage: cropped.cgImage!)
        let req = VNRecognizeTextRequest(completionHandler: textHandler)
        do {
            try handler.perform([req])
        } catch {
            stopLoading(progress)
        }
        cropper.dismiss(animated: true, completion: nil)
    }
}

class ReviewPhotoController: SimpleController, UITextViewDelegate, UIContextMenuInteractionDelegate {
    @IBOutlet weak var image: UIImageView!
    @IBOutlet weak var text: UITextView!
    
    var scrapeInfo: ScrapeImageInfo? = nil
    var navigationVc: AddController? = nil
    var addVc: AddPhotoController? = nil
    
    @IBAction func didTapBack(_ sender: Any?) {
        DispatchQueue.main.async {
            self.navigationVc?.popViewController(animated: true)
        }
    }
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        if let info = scrapeInfo {
            switch info.type {
            case .ingredient:
                addVc?.scrape.ingredients.append(info)
                break
            case .step:
                addVc?.scrape.steps.append(info)
                break
            }
        }
        DispatchQueue.main.async {
            self.navigationVc?.popViewController(animated: true)
            self.addVc?.pickVc?.collectionView.reloadData()
        }
    }
    
    func contextMenuInteraction(_ interaction: UIContextMenuInteraction, configurationForMenuAtLocation location: CGPoint) -> UIContextMenuConfiguration? {
        guard let image = scrapeInfo?.image else { return nil }
        return UIContextMenuConfiguration(identifier: nil, previewProvider: {
            return PreviewPhotoController(image: image)
        }, actionProvider: nil)
    }
    
    func textViewDidEndEditing(_ textView: UITextView) {
        scrapeInfo?.value = textView.text
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        text.text = scrapeInfo?.value
        text.addDoneButtonOnKeyboard()
        text.layer.cornerRadius = 10
        image.addInteraction(UIContextMenuInteraction(delegate: self))
        image.image = scrapeInfo?.image
        image.layer.cornerRadius = 10
    }
}

class PreviewPhotoController: UIViewController {
    private let imageView = UIImageView()

    override func loadView() {
        view = imageView
    }

    init(image: UIImage) {
        super.init(nibName: nil, bundle: nil)
        imageView.clipsToBounds = true
        imageView.contentMode = .scaleAspectFill
        imageView.image = image
        preferredContentSize = image.size
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
}
