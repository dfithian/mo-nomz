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

enum ScrapeType {
    case ingredient
    case step
}

struct ScrapeInfo {
    var image: UIImage
    var value: String
    let type: ScrapeType
}

struct Scrape {
    var ingredients: [ScrapeInfo]
    var steps: [ScrapeInfo]
}

class AddPhotoController: UIViewController {
    @IBOutlet weak var switcher: UIBarButtonItem!

    var scrape: Scrape = Scrape(ingredients: [], steps: [])
    var current: ScrapeInfo? = nil
    var navigationVc: AddController? = nil
    var pickVc: PickPhotoController? = nil
    
    @IBAction func didTapCancel(_ sender: Any?) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        performSegue(withIdentifier: "pushManualRecipe", sender: nil)
    }
    
    private func setupSwitcher() {
        switcher.menu = UIMenu(options: .displayInline, children: [
            UIAction(title: "Add link", image: UIImage(systemName: "link"), state: .off, handler: { _ in self.navigationVc?.switchToLink() }),
            UIAction(title: "Add manual", image: UIImage(systemName: "pencil"), state: .off, handler: { _ in self.navigationVc?.switchToManual() }),
            UIAction(title: "Add photos", image: UIImage(systemName: "photo.on.rectangle"), state: .on, handler: { _ in self.navigationVc?.switchToPhoto() })
        ])
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? PickPhotoController, segue.identifier == "embedPhotos" {
            pickVc = vc
            vc.addVc = self
        }
        if let vc = segue.destination as? AddManualController, segue.identifier == "pushManualRecipe" {
            vc.change = .photoReview
            vc.isActive = true
            vc.ingredients = scrape.ingredients.map({ $0.value }).joined(separator: "\n")
            if scrape.steps.isEmpty {
                vc.isRecipe = false
            } else {
                vc.isRecipe = true
                vc.steps = scrape.steps.map({ $0.value }).joined(separator: "\n")
            }
        }
        if let vc = segue.destination as? ReviewPhotoController, segue.identifier == "reviewScrape" {
            vc.addVc = self
            vc.scrapeInfo = current
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        setupSwitcher()
    }
}

class PickPhotoController: UICollectionViewController, UICollectionViewDelegateFlowLayout, UIImagePickerControllerDelegate, UINavigationControllerDelegate {
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
    
    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        switch indexPath.section {
        case INGREDIENTS:
            if indexPath.row < addVc!.scrape.ingredients.count {
                let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "imageItem", for: indexPath) as! OneCellImage
                cell.layer.cornerRadius = 10
                cell.image.image = addVc!.scrape.ingredients[indexPath.row].image
                return cell
            } else {
                let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "addImage", for: indexPath) as! OneCellButton
                cell.layer.cornerRadius = 10
                cell.button.tag = INGREDIENTS
                cell.button.addTarget(self, action: #selector(didTapAddPhoto), for: .touchUpInside)
                return cell
            }
        case STEPS:
            if indexPath.row < addVc!.scrape.steps.count {
                let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "imageItem", for: indexPath) as! OneCellImage
                cell.layer.cornerRadius = 10
                cell.image.image = addVc!.scrape.steps[indexPath.row].image
                return cell
            } else {
                let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "addImage", for: indexPath) as! OneCellButton
                cell.layer.cornerRadius = 10
                cell.button.tag = STEPS
                cell.button.addTarget(self, action: #selector(didTapAddPhoto), for: .touchUpInside)
                return cell
            }
        default: return UICollectionViewCell()
        }
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
                UIAction(title: "Remove", attributes: .destructive, handler: { _ in
                    handler()
                    self.collectionView.reloadData()
                })
            ])
        }
        return UIContextMenuConfiguration(identifier: nil, previewProvider: preview, actionProvider: actions)
    }
    
    override func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        switch indexPath.section {
        case INGREDIENTS:
            if indexPath.row < addVc!.scrape.ingredients.count {
                self.addVc?.current = addVc?.scrape.ingredients[indexPath.row]
                self.addVc?.performSegue(withIdentifier: "reviewScrape", sender: nil)
            } else {
                let cell = collectionView.cellForItem(at: indexPath) as! OneCellButton
                didTapAddPhoto(cell.button)
            }
            break
        case STEPS:
            if indexPath.row < addVc!.scrape.steps.count {
                self.addVc?.current = addVc?.scrape.steps[indexPath.row]
                self.addVc?.performSegue(withIdentifier: "reviewScrape", sender: nil)
            } else {
                let cell = collectionView.cellForItem(at: indexPath) as! OneCellButton
                didTapAddPhoto(cell.button)
            }
            break
        default: break
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
        let progress = startLoading()
        picker.dismiss(animated: true, completion: nil)
        if let image = info[.originalImage] as? UIImage {
            let textHandler = { (req: VNRequest, error: Error?) in
                self.stopLoading(progress)
                guard let observations = req.results as? [VNRecognizedTextObservation] else { return }
                let recognized = observations.compactMap { $0.topCandidates(1).first?.string }
                self.addVc?.current = ScrapeInfo(image: image, value: recognized.joined(separator: "\n"), type: self.scrapeType ?? .ingredient)
                self.addVc?.performSegue(withIdentifier: "reviewScrape", sender: nil)
            }
            let handler = VNImageRequestHandler(cgImage: image.cgImage!)
            let req = VNRecognizeTextRequest(completionHandler: textHandler)
            do {
                try handler.perform([req])
            } catch {
                defaultOnError(error)
            }
        }
    }
}

class ReviewPhotoController: UIViewController, UITextViewDelegate, UIContextMenuInteractionDelegate {
    @IBOutlet weak var help: UILabel!
    @IBOutlet weak var image: UIImageView!
    @IBOutlet weak var text: UITextView!
    
    var scrapeInfo: ScrapeInfo? = nil
    var navigationVc: AddController? = nil
    var addVc: AddPhotoController? = nil
    var beforeHeight: CGFloat? = nil
    
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
    
    @objc func keyboardWillShow(notification: NSNotification) {
        beforeHeight = keyboardWillShowInternal(subview: text, notification: notification)
    }
    
    @objc func keyboardWillHide(notification: NSNotification) {
        keyboardWillHideInternal(heightMay: beforeHeight, notification: notification)
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        switch scrapeInfo?.type {
        case .ingredient:
            help.text = "Review ingredients"
            break
        case .step:
            help.text = "Review steps"
            break
        default: break
        }
        text.text = scrapeInfo?.value
        text.addDoneButtonOnKeyboard()
        text.layer.cornerRadius = 10
        image.addInteraction(UIContextMenuInteraction(delegate: self))
        image.image = scrapeInfo?.image
        image.layer.cornerRadius = 10
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow), name: UIResponder.keyboardDidShowNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillHide), name: UIResponder.keyboardWillHideNotification, object: nil)
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
